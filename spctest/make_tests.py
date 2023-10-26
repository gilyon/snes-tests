#!/usr/bin/env python3

TESTS_PER_FILE = 500

test_num = 0
file_num = 0
tests_txt = []
spc_code = []

FLAGS_MAP = {'C': 1, 'Z': 2, 'I': 4, 'H': 8, 'B': 0x10, 'P': 0x20, 'V': 0x40, 'N': 0x80}

# addressing modes:
A_IMPLIED = 0
A_DP = 1  # $12
A_DP_X = 2  # $12+x
A_DP_Y = 3  # $12+y
A_IND_X = 4  # (x)
A_DP_DP = 5  # $12, $34
A_IND_X_IND_Y = 6  # (x), (y)
A_DP_IMM = 7  # $12, #$34
A_ABS = 8,  # $1234
A_ABS_X = 9,  # $1234+x
A_ABS_Y = 10,  # $1234+y
A_DP_X_IND = 11,  # [dp+x]
A_IND_DP_Y = 12,  # [dp]+y
A_IMM = 13,  # #$12
A_IND_X_PLUS = 14,  # (x)+

# Other modes are handled with specific instructions' tests


def flags(p, default=None, add=None):
    ''' Return flags byte, converting from string and adding `add` if needed'''
    if p is None:
        return flags(default or 0, add=add)
    if isinstance(p, int):
        ret = p
    else:
        ret = sum(FLAGS_MAP[c] for c in set(p))
    if add is not None:
        ret |= flags(add)
    return ret


def test(ins, a=0x12, x=0x34, y=0x56, p=None, ea=None, ex=None, ey=None, ep=None, esp=None,
         before_regs=None, before_ins=None, after_ins=None, after_checks=None, **kwargs):
    '''Test the given instruction, setting registers/memory before and checking them after.

    - a, x, y, p are the input register values
    - e* are the expected output values. If None then they should be the same as input.
    - mem_abcd means this is the input value in memory location $ABCD (or any other).
    - emem_abcd is the output value.
    - before_regs, before_ins, after_ins, after_checks are used to add additional assembly code
      at specific points.
    '''
    global test_num
    if test_num % TESTS_PER_FILE == 0 and test_num != 0:
        close_spc_file()
        start_spc_file()

    p = flags(p)
    ep = flags(ep, p)
    if ea is None:
        ea = a
    if ex is None:
        ex = x
    if ey is None:
        ey = y

    mem_input = []
    mem_output = {}
    for (k, v) in kwargs.items():
        if k.startswith('mem_'):
            mem_input.append((int(k[4:], 16), v))
        elif k.startswith('emem_'):
            mem_output[int(k[5:], 16)] = v
        else:
            assert False, 'Invalid keyword: ' + k

    spc_code.append(f'''test{test_num:04x}:
        mov a, #${(test_num & 0xFF):02x}
        mov y, #${(test_num >> 8):02x}
        call init_test''')

    if before_regs:
        spc_code.append(before_regs)
    for loc, val in mem_input:
        if loc not in mem_output:
            mem_output[loc] = val
        if loc < 0x100:
            spc_code.append(f'        mov ${loc:02x}, #${val:02x}')
        else:
            spc_code.append(f'        mov a, #${val:02x}')
            spc_code.append(f'        mov ${loc:04x}, a')

    spc_code.append(f'''\
        mov a, #${p:02x}
        push a
        mov a, #${a:02x}
        mov x, #${x:02x}
        mov y, #${y:02x}
        pop psw''')
    if before_ins:
        spc_code.append(before_ins)
    spc_code.append(f'        {ins}')
    if after_ins:
        spc_code.append(after_ins)
    spc_code.append(f'''\
        call save_results ; now a = result psw
        cmp a, #${ep:02x}
        bne .to_fail
        cmp result_a, #${ea:02x}
        bne .to_fail
        cmp x, #${ex:02x}
        bne .to_fail
        cmp y, #${ey:02x}
        bne .to_fail''')
    if esp is not None:
        spc_code.append(f'''\
        mov x, sp
        cmp x, #${esp:02x}
        bne .to_fail''')

    for loc, val in mem_output.items():
        if val is not None:
            if loc < 0x100:
                spc_code.append(f'        cmp ${loc:02x}, #${val:02x}')
            else:
                spc_code.append(f'        mov a, ${loc:04x}')
                spc_code.append(f'        cmp a, #${val:02x}')
            spc_code.append('        bne .to_fail')

    if after_checks:
        spc_code.append(after_checks)

    spc_code.append('''\
        bra .next_test
    .to_fail:
        jmp fail
    .next_test:
    ''')

    tests_txt.append(f'Test {test_num:04x}: {ins}')
    inputs = f'A=${a:02x} X=${x:02x} Y=${y:02x} P=${p:02x}'
    if mem_input:
        inputs += ' ' + ' '.join(f'(${loc:02x})=${val:02x}' for loc, val in mem_input)
    outputs = f'A=${ea:02x} X=${ex:02x} Y=${ey:02x} P=${ep:02x}'
    if esp is not None:
        outputs += f' SP={esp:02x}'
    if mem_output:
        outputs += ' ' + ' '.join(f'(${loc:02x})=${val:02x}' for loc, val in mem_output.items() if val is not None)

    tests_txt.append(f'   Input: {inputs}')
    tests_txt.append(f'   Expected output: {outputs}')
    if before_regs or before_ins or after_ins or after_checks:
        tests_txt.append('   Additional initialization or checks are performed - see assembly')

    test_num += 1


def add_operand(base_ins, operand):
    if '%' in base_ins:
        return base_ins.replace('%', operand)
    else:
        return f'{base_ins} {operand}'


def test_one_param_ins(base_ins, modes, val, result=None, p='', ep=None, **kwargs):
    ''' Test an instruction with one operand such as ASL.
    If base_ins contains a % sign then the operand replaces it, otherwise it's placed at the end.
    '''
    if result is None:
        result = val

    def do_test(operand, add_p_flag=False, **kwargs2):
        add_flags = flags('P') if add_p_flag else 0
        new_p = flags(p) | add_flags
        new_ep = flags(ep, new_p) | add_flags
        kwargs3 = kwargs.copy()
        kwargs3.update(kwargs2)
        test(add_operand(base_ins, operand), p=new_p, ep=new_ep, **kwargs3)

    for mode in modes:
        if mode == A_IMPLIED:
            do_test('a', a=val, ea=result)
        elif mode == A_DP:
            do_test('$01', mem_01=val, emem_01=result)
            do_test('$02', add_p_flag=True, mem_102=val, emem_102=result)
        elif mode == A_DP_X:
            do_test('$ff+x', x=2, mem_01=val, emem_01=result)
            do_test('$ff+x', add_p_flag=True, x=3, mem_102=val, emem_102=result)
        elif mode == A_DP_Y:
            do_test('$ff+y', y=2, mem_01=val, emem_01=result)
            do_test('$ff+y', add_p_flag=True, y=3, mem_102=val, emem_102=result)
        elif mode == A_ABS:
            do_test('$103', mem_103=val, emem_103=result)
        elif mode == A_IND_X:
            do_test('(x)', x=1, mem_01=val, emem_01=result)
        elif mode == A_IND_X_PLUS:
            do_test('(x+)', x=1, ex=2, mem_01=val, emem_01=result)   # spcasm doesn't support (x)+ syntax for some reason
        elif mode == A_ABS_X:
            do_test('$ffff+x', x=5, mem_04=val, emem_04=result)
        elif mode == A_ABS_Y:
            do_test('$103+y', y=1, mem_104=val, emem_104=result)
        elif mode == A_DP_X_IND:
            do_test('[$ff+x]', x=2, mem_01=0x02, mem_02=0x01, mem_102=val, emem_102=result)
        elif mode == A_IND_DP_Y:
            do_test('[$01]+y', y=5, mem_01=0xfe, mem_02=0xff, mem_03=val, emem_03=result)
        elif mode == A_IMM:
            assert val == result  # Can't change imm...
            do_test(f'#${val:02x}')
        else:
            assert False, 'Invalid mode for ' + base_ins


def test_two_param_ins(base_ins, modes, val1, val2, result=None, p='', ep=None):
    ''' Test an instruction with two operands such as AND'''
    if result is None:
        result = val1
    for mode in modes:
        if mode == A_DP_DP:
            test(f'{base_ins} $01, $02', mem_01=val1, mem_02=val2, emem_01=result, p=p, ep=ep)
            test(f'{base_ins} $02, $03', mem_102=val1, mem_103=val2, emem_102=result, p=flags(p, add='P'), ep=flags(ep, p, add='P'))
        elif mode == A_IND_X_IND_Y:
            test(f'{base_ins} (x), (y)', x=1, y=2, mem_01=val1, mem_02=val2, emem_01=result, p=p, ep=ep)
        elif mode == A_DP_IMM:
            test_one_param_ins(f'{base_ins} %, #${val2:02x}', [A_DP], val1, result, p=p, ep=ep)
        else:
            # It's a single operand, so add a fixed A operand and treated the second one as the only operand
            test_one_param_ins(f'{base_ins} a, %', [mode], val2, a=val1, ea=result, p=p, ep=ep)


def test_16bit_ins(base_ins, val, result=None, ya=None, p='', eya=None, ep=None, **kwargs):
    '''Test a 16 bit instruction. These only use direct page addressing'''
    if result is None:
        result = val
    if ya is not None:
        kwargs['a'] = ya & 0xFF
        kwargs['y'] = ya >> 8
    if eya is not None:
        kwargs['ea'] = eya & 0xFF
        kwargs['ey'] = eya >> 8
    test(add_operand(base_ins, '$01'), mem_01=val & 0xFF, mem_02=val >> 8, emem_01=result & 0xFF, emem_02=result >> 8,
         p=p, ep=ep, **kwargs)
    # now with direct page 1 + wrapping
    test(add_operand(base_ins, '$FF'), mem_1ff=val & 0xFF, mem_100=val >> 8, emem_1ff=result & 0xFF, emem_100=result >> 8,
         p=flags(p, add='P'), ep=flags(ep, p, add='P'), **kwargs)


def test_branch_taken(base_ins, **kwargs):
    # branch backwards:
    before = '''\
        bra .branch
    .ok:
        bra .ok2
    .branch:'''
    after = '''\
        call save_results
        bra .to_fail
    .ok2:'''
    test(f'{base_ins} .ok', before_ins=before, after_ins=after, **kwargs)

    # branch forward:
    after = '''\
        call save_results
        bra .to_fail
    .ok:'''
    test(f'{base_ins} .ok', after_ins=after, **kwargs)


def test_branch_not_taken(base_ins, **kwargs):
    after = '''\
        bra .ok
    .not_ok:
        call save_results
        bra .to_fail
    .ok:'''
    test(f'{base_ins} .not_ok', after_ins=after, **kwargs)


def adc_tests():
    modes = [A_IMM, A_IND_X_IND_Y, A_IND_X, A_IND_DP_Y, A_DP_X_IND, A_DP, A_DP_X, A_ABS, A_ABS_X, A_ABS_Y, A_DP_DP, A_DP_IMM]
    test_two_param_ins('adc', modes, 0x12, 0x34, p='', result=0x46, ep='')
    test_two_param_ins('adc', modes, 0x12, 0x34, p='CZIHBVN', result=0x47, ep='IB')
    test_two_param_ins('adc', modes, 0xCB, 0x34, p='C', result=0x00, ep='ZCH')
    test_two_param_ins('adc', modes, 0x00, 0x00, p='', result=0x00, ep='Z')
    test_two_param_ins('adc', modes, 0x90, 0x90, p='', result=0x20, ep='CV')
    test_two_param_ins('adc', modes, 0x50, 0x50, p='', result=0xA0, ep='VN')
    test_two_param_ins('adc', modes, 0x00, 0x7F, p='C', result=0x80, ep='VNH')
    test_two_param_ins('adc', modes, 0x00, 0xFF, p='C', result=0x00, ep='ZCH')


def addw_tests():
    def test_addw(val1, val2, p, ep):
        res = (val1 + val2) & 0xFFFF
        test_16bit_ins('addw ya, %', val2, ya=val1, p=p, eya=res, ep=ep)

    test_addw(0x3412, 0x00ff, p='', ep='')
    test_addw(0x3412, 0x00ff, p='CZIHBVN', ep='IB')
    test_addw(0x0000, 0x0001, p='', ep='')
    test_addw(0x0000, 0x0100, p='', ep='')
    test_addw(0x34ff, 0x00ff, p='H', ep='')
    test_addw(0x3412, 0x0C00, p='', ep='H')
    test_addw(0x7fff, 0x0001, p='', ep='HNV')
    test_addw(0xffff, 0x0001, p='', ep='HZC')
    test_addw(0x8000, 0xffff, p='', ep='CV')


def and_tests():
    modes = [A_IMM, A_IND_X_IND_Y, A_IND_X, A_IND_DP_Y, A_DP_X_IND, A_DP, A_DP_X, A_ABS, A_ABS_X, A_ABS_Y, A_DP_DP, A_DP_IMM]
    test_two_param_ins('and', modes, 0xff, 0xff, result=0xff, ep='N')
    test_two_param_ins('and', modes, 0xff, 0x00, result=0x00, ep='Z')
    test_two_param_ins('and', modes, 0x00, 0xff, result=0x00, p='CZIHBVN', ep='CZIHBV')
    test_two_param_ins('and', modes, 0xC5, 0x93, result=0x81, p='CZIHBVN', ep='CIHBVN')


def and1_tests():
    test('and1 C, $102.2', p='C', mem_102=0x04, ep='C')
    test('and1 C, $102.2', p='', mem_102=0x04, ep='')
    test('and1 C, $102.2', p='CZIHBPVN', mem_102=0xFB, ep='ZIHBPVN')
    test('and1 C, /$102.2', p='C', mem_102=0x04, ep='')
    test('and1 C, /$102.2', p='C', mem_102=0xFB, ep='C')
    test('and1 C, /$102.2', p='C', mem_102=0x00, ep='C')


def asl_tests():
    modes = [A_IMPLIED, A_DP, A_DP_X, A_ABS]
    test_one_param_ins('asl', modes, 0xFF, 0xFE, p='', ep='NC')
    test_one_param_ins('asl', modes, 0x80, 0x00, p='', ep='ZC')
    test_one_param_ins('asl', modes, 0x03, 0x06, p='CZIHBVN', ep='IHBV')


def bbc_tests():
    for bit in range(8):
        test_branch_not_taken(f'bbc $01.{bit},', mem_01=1 << bit)
        test_branch_not_taken(f'bbc $01.{bit},', mem_01=0xff, p='CZIHBVN')
        test_branch_taken(f'bbc $01.{bit},', mem_01=0xFF - (1 << bit))
        test_branch_taken(f'bbc $01.{bit},', mem_01=0x00, p='CZIHBVN')
        test_branch_not_taken(f'bbc $02.{bit},', mem_102=1 << bit, p='P')
        test_branch_taken(f'bbc $02.{bit},', mem_102=0xFF - (1 << bit), p='P')


def bbs_tests():
    for bit in range(8):
        test_branch_taken(f'bbs $01.{bit},', mem_01=1 << bit)
        test_branch_taken(f'bbs $01.{bit},', mem_01=0xff, p='CZIHBVN')
        test_branch_not_taken(f'bbs $01.{bit},', mem_01=0xFF - (1 << bit))
        test_branch_not_taken(f'bbs $01.{bit},', mem_01=0x00, p='CZIHBVN')
        test_branch_taken(f'bbs $02.{bit},', mem_102=1 << bit, p='P')
        test_branch_not_taken(f'bbs $02.{bit},', mem_102=0xFF - (1 << bit), p='P')


def branch_tests():
    ''' Tests the simple branch instructions which don't access memory'''
    test_branch_taken('bra')
    test_branch_taken('bra', p='CZIHBPVN')

    def flag_test(ins_on, ins_off, flag):
        flag = flags(flag)
        test_branch_taken(ins_on, p=flag)
        test_branch_not_taken(ins_off, p=flag)
        test_branch_not_taken(ins_on, p='')
        test_branch_taken(ins_off, p='')
        test_branch_taken(ins_on, p='CZIHBPVN')
        test_branch_not_taken(ins_off, p='CZIHBPVN')
        test_branch_not_taken(ins_on, p=flags('CZIHBPVN') - flag)
        test_branch_taken(ins_off, p=flags('CZIHBPVN') - flag)

    flag_test('bcs', 'bcc', 'C')
    flag_test('beq', 'bne', 'Z')
    flag_test('bmi', 'bpl', 'N')
    flag_test('bvs', 'bvc', 'V')


def brk_tests():
    before_regs = '''\
        mov a, #(.brk_target & $FF)
        mov $ffde, a
        mov a, #(.brk_target >> 8)
        mov $ffdf, a'''

    after_ins = '''\
    .after_brk:
        bra .to_fail
    .brk_target:'''

    after_checks = '''\
        mov a, $1ee
        cmp a, #(.after_brk & $FF)
        bne .to_fail
        mov a, $1ef
        cmp a, #(.after_brk >> 8)
        bne .to_fail
        pop a
        pop a
        pop a'''

    test('brk', p='', ep='B', esp=0xEC, emem_1ed=0x00, before_regs=before_regs, after_ins=after_ins, after_checks=after_checks)
    test('brk', p='CZIHBPVN', ep='CZHBPVN', esp=0xEC, emem_1ed=flags('CZIHBPVN'), before_regs=before_regs, after_ins=after_ins, after_checks=after_checks)


def call_tests():
    after_ins = '''\
    .after_call:
        bra .to_fail
    .call_target:'''

    after_checks = '''\
        mov a, $1ee
        cmp a, #(.after_call & $FF)
        bne .to_fail
        mov a, $1ef
        cmp a, #(.after_call >> 8)
        bne .to_fail
        pop a
        pop a'''

    test('call .call_target', after_ins=after_ins, after_checks=after_checks)
    test('call .call_target', p='CZIHBPVN', after_ins=after_ins, after_checks=after_checks)

    # put 'jmp .call_target' at $FFFF (wrapping)
    pcall_before_regs = '''\
        mov a, #$5F
        mov $FFFF, a
        mov $00, #(.call_target & $FF)
        mov $01, #(.call_target >> 8)'''

    test('pcall $FF', before_regs=pcall_before_regs, after_ins=after_ins, after_checks=after_checks)
    test('pcall $FF', p='CZIHBPVN', before_regs=pcall_before_regs, after_ins=after_ins, after_checks=after_checks)

    for n in range(16):
        addr = 0xFFDE - 2 * n
        # erase previous pointer to make sure we don't use it, and add a new one.
        tcall_before_regs = f'''\
            mov a, #$00
            mov ${addr-1:04x}, a
            mov a, #(.call_target & $FF)
            mov ${addr:04x}, a
            mov a, #(.call_target >> 8)
            mov ${addr+1:04x}, a'''
        test(f'tcall {n}', before_regs=tcall_before_regs, after_ins=after_ins, after_checks=after_checks)
        test(f'tcall {n}', p='CZIHBPVN', before_regs=tcall_before_regs, after_ins=after_ins, after_checks=after_checks)


def cbne_tests():
    for p in ['', 'CZIHBVN']:
        test_branch_taken('cbne $01,', p=p, ep=p, a=0x12, mem_01=0x13)
        test_branch_not_taken('cbne $01,', p=p, ep=p, a=0x80, mem_01=0x80)
        test_branch_taken('cbne $ff+x,', p=p, ep=p, x=2, a=0xff, mem_01=0x00)
        test_branch_not_taken('cbne $ff+x,', p=p, ep=p, x=2, a=0xff, mem_01=0xff)

        test_branch_taken('cbne $02,', p=p + 'P', ep=p + 'P', a=0x12, mem_102=0x13)
        test_branch_not_taken('cbne $02,', p=p + 'P', ep=p + 'P', a=0x80, mem_102=0x80)
        test_branch_taken('cbne $ff+x,', p=p + 'P', ep=p + 'P', x=4, a=0xff, mem_103=0x00)
        test_branch_not_taken('cbne $ff+x,', p=p + 'P', ep=p + 'P', x=4, a=0xff, mem_103=0xff)


def clr1_tests():
    for bit in range(8):
        test_one_param_ins(f'clr1 %.{bit}', [A_DP], 0xff, 0xff - (1 << bit))
        test_one_param_ins(f'clr1 %.{bit}', [A_DP], 0x00, 0x00)
        test_one_param_ins(f'clr1 %.{bit}', [A_DP], 0xff, 0xff - (1 << bit), p='CZIHBVN', ep='CZIHBVN')
        test_one_param_ins(f'clr1 %.{bit}', [A_DP], 0x00, 0x00, p='CZIHBVN', ep='CZIHBVN')


def flag_manip_tests():
    test('clrc', p='CZIHBPVN', ep='ZIHBPVN')
    test('clrc', p='', ep='')
    test('clrp', p='CZIHBPVN', ep='CZIHBVN')
    test('clrp', p='', ep='')
    test('clrv', p='CZIHBPVN', ep='CZIBPN')
    test('clrv', p='', ep='')
    test('setc', p='CZIHBPVN', ep='CZIHBPVN')
    test('setc', p='', ep='C')
    test('setp', p='CZIHBPVN', ep='CZIHBPVN')
    test('setp', p='', ep='P')
    test('di', p='CZIHBPVN', ep='CZHBPVN')
    test('di', p='', ep='')
    test('ei', p='CZIHBPVN', ep='CZIHBPVN')
    test('ei', p='', ep='I')
    test('notc', p='CZIHBPVN', ep='ZIHBPVN')
    test('notc', p='', ep='C')


def cmp_tests():
    modes = [A_IMM, A_IND_X_IND_Y, A_IND_X, A_IND_DP_Y, A_DP_X_IND, A_DP, A_DP_X, A_ABS, A_ABS_X, A_ABS_Y, A_DP_DP, A_DP_IMM]
    modes_xy = [A_IMM, A_DP, A_ABS]  # addressing modes for comparing with x or y registers

    def do_test(val1, val2, p, ep):
        test_two_param_ins('cmp', modes, val1, val2, p=p, ep=ep)
        test_one_param_ins('cmp x, %', modes_xy, val2, x=val1, p=p, ep=ep)
        test_one_param_ins('cmp y, %', modes_xy, val2, y=val1, p=p, ep=ep)

    do_test(0x12, 0x12, p='', ep='ZC')
    do_test(0x13, 0x12, p='CZIHBVN', ep='CIHBV')
    do_test(0x12, 0x13, p='CZIHBVN', ep='IHBVN')
    do_test(0xfe, 0xff, p='', ep='N')
    do_test(0xfe, 0x01, p='', ep='CN')


def cmpw_tests():
    test_16bit_ins('cmpw ya, %', ya=0x2345, val=0x1245, p='', ep='C')
    test_16bit_ins('cmpw ya, %', ya=0xffff, val=0xffff, p='CZIHBVN', ep='CZIHBV')
    test_16bit_ins('cmpw ya, %', ya=0xffff, val=1, p='CZIHBVN', ep='CIHBVN')
    test_16bit_ins('cmpw ya, %', ya=0x7fff, val=0x8000, p='', ep='N')


def daa_tests():
    test('daa a', a=0x10, p='', ea=0x10, ep='')
    test('daa a', a=0x1A, p='', ea=0x20, ep='')
    test('daa a', a=0x10, p='C', ea=0x70, ep='C')
    test('daa a', a=0x10, p='CH', ea=0x76, ep='CH')
    test('daa a', a=0xFF, p='ZIBPVN', ea=0x65, ep='CIBPV')
    test('daa a', a=0x9A, p='', ea=0x00, ep='ZC')

    # The following can only happen after addition of numbers with non-BCD digits
    test('daa a', a=0x91, p='C', ea=0xF1, ep='CN')
    test('daa a', a=0x9A, p='H', ea=0x00, ep='ZHC')
    test('daa a', a=0x99, p='H', ea=0x9F, ep='HN')


def das_tests():
    test('das a', a=0x10, p='CH', ea=0x10, ep='CH')
    test('das a', a=0x1A, p='CH', ea=0x14, ep='CH')
    test('das a', a=0xA1, p='CZIHBPVN', ea=0x41, ep='IHBPV')
    test('das a', a=0xFF, p='CH', ea=0x99, ep='HN')
    test('das a', a=0x99, p='', ea=0x33, ep='')
    test('das a', a=0x66, p='', ea=0x00, ep='Z')

    # The following can only happen after subtraction of numbers with non-BCD digits
    test('das a', a=0x11, p='', ea=0xAB, ep='N')


def dbnz_tests():
    test_branch_taken('dbnz y,', y=0, ey=0xff, p='')
    test_branch_taken('dbnz y,', y=0x81, ey=0x80, p='CZIHBPVN', ep='CZIHBPVN')
    test_branch_not_taken('dbnz y,', y=1, ey=0, p='')

    test_branch_taken('dbnz $01,', mem_01=0, emem_01=0xff, p='')
    test_branch_taken('dbnz $01,', mem_01=0x81, emem_01=0x80, p='CZIHBVN', ep='CZIHBVN')
    test_branch_not_taken('dbnz $01,', mem_01=1, emem_01=0, p='')

    test_branch_taken('dbnz $02,', mem_102=0, emem_102=0xff, p='P')
    test_branch_taken('dbnz $02,', mem_102=0x81, emem_102=0x80, p='CZIHBPVN', ep='CZIHBPVN')
    test_branch_not_taken('dbnz $02,', mem_102=1, emem_102=0, p='P')


def dec_tests():
    modes = [A_IMPLIED, A_DP, A_DP_X, A_ABS]

    def do_test(val, res, p, ep):
        test_one_param_ins('dec', modes, val, res, p=p, ep=ep)
        test('dec x', x=val, ex=res, p=p, ep=ep)
        test('dec y', y=val, ey=res, p=p, ep=ep)

    do_test(0x00, 0xFF, p='', ep='N')
    do_test(0x01, 0x00, p='', ep='Z')
    do_test(0x80, 0x7F, p='CZIHBVN', ep='CIHBV')


def decw_tests():
    test_16bit_ins('decw', 0x0001, 0x0000, p='', ep='Z')
    test_16bit_ins('decw', 0x0000, 0xFFFF, p='', ep='N')
    test_16bit_ins('decw', 0x0101, 0x0100, p='CZIHBVN', ep='CIHBV')
    test_16bit_ins('decw', 0x0080, 0x007F, p='', ep='')


def div_tests():
    test('div ya, x', y=0x00, a=0x00, x=0x00, p='', ey=0, ea=0xff, ep='HVN')
    test('div ya, x', y=0x01, a=0x23, x=0x10, p='', ey=3, ea=0x12, ep='H')
    test('div ya, x', y=0x01, a=0x10, x=0x88, p='CZIHBPVN', ey=0, ea=0x2, ep='CIBP')
    test('div ya, x', y=0x01, a=0x0f, x=0x88, p='CZIHBPVN', ey=0x87, ea=0x1, ep='CIBP')
    test('div ya, x', y=0x01, a=0x23, x=0x01, p='', ey=0, ea=0x23, ep='HV')
    test('div ya, x', y=0xff, a=0xff, x=0x00, p='', ey=0xff, ea=0x00, ep='ZVH')
    test('div ya, x', y=0xab, a=0xcd, x=0x03, p='', ey=0xc5, ea=0x58, ep='VH')


def eor_tests():
    modes = [A_IMM, A_IND_X_IND_Y, A_IND_X, A_IND_DP_Y, A_DP_X_IND, A_DP, A_DP_X, A_ABS, A_ABS_X, A_ABS_Y, A_DP_DP, A_DP_IMM]
    test_two_param_ins('eor', modes, 0xff, 0xff, result=0x00, ep='Z')
    test_two_param_ins('eor', modes, 0xff, 0x00, result=0xff, ep='N')
    test_two_param_ins('eor', modes, 0x55, 0xff, result=0xaa, p='CZIHBVN', ep='CIHBVN')
    test_two_param_ins('eor', modes, 0x35, 0x89, result=0xbc, p='CZIHBVN', ep='CIHBVN')


def eor1_tests():
    test('eor1 C, $102.2', p='C', mem_102=0x04, ep='')
    test('eor1 C, $102.2', p='', mem_102=0x04, ep='C')
    test('eor1 C, $102.2', p='CZIHBPVN', mem_102=0xFB, ep='CZIHBPVN')
    test('eor1 C, $102.2', p='ZIHBPVN', mem_102=0xFB, ep='ZIHBPVN')


def inc_tests():
    modes = [A_IMPLIED, A_DP, A_DP_X, A_ABS]

    def do_test(val, res, p, ep):
        test_one_param_ins('inc', modes, val, res, p=p, ep=ep)
        test('inc x', x=val, ex=res, p=p, ep=ep)
        test('inc y', y=val, ey=res, p=p, ep=ep)

    do_test(0x00, 0x01, p='', ep='')
    do_test(0xFF, 0x00, p='', ep='Z')
    do_test(0x80, 0x81, p='CZIHBVN', ep='CIHBVN')


def incw_tests():
    test_16bit_ins('incw', 0xFFFF, 0x0000, p='', ep='Z')
    test_16bit_ins('incw', 0x0000, 0x0001, p='', ep='')
    test_16bit_ins('incw', 0x00FF, 0x0100, p='CZIHBVN', ep='CIHBV')
    test_16bit_ins('incw', 0x7FFF, 0x8000, p='', ep='N')


def jmp_tests():
    after = '''\
        call save_results
        bra .to_fail
    .ok:'''

    test('jmp .ok', after_ins=after)

    before = '''\
        mov a, #(.ok & $FF)
        mov $FFFF, a
        mov $00, #(.ok >> 8)'''
    test('jmp [$FF00+x]', x=0xff, before_regs=before, after_ins=after)


def lsr_tests():
    modes = [A_IMPLIED, A_DP, A_DP_X, A_ABS]
    test_one_param_ins('lsr', modes, 0xFF, 0x7F, p='', ep='C')
    test_one_param_ins('lsr', modes, 0x01, 0x00, p='', ep='ZC')
    test_one_param_ins('lsr', modes, 0x06, 0x03, p='CZIHBVN', ep='IHBV')


def mov_tests():
    # mov a, mem/imm
    modes = [A_IMM, A_IND_X, A_IND_DP_Y, A_DP_X_IND, A_DP, A_DP_X, A_ABS, A_ABS_X, A_ABS_Y, A_IND_X_PLUS]
    test_one_param_ins('mov a, %', modes, 0x12, a=0x00, ea=0x12, p='CZIHBVN', ep='CIHBV')
    test_one_param_ins('mov a, %', modes, 0x80, ea=0x80, p='', ep='N')
    test_one_param_ins('mov a, %', modes, 0x00, ea=0x00, p='', ep='Z')

    # mov mem, a
    modes = [A_IND_X, A_IND_DP_Y, A_DP_X_IND, A_DP, A_DP_X, A_ABS, A_ABS_X, A_ABS_Y, A_IND_X_PLUS]
    test_one_param_ins('mov %, a', modes, 0x00, a=0x12, result=0x12, p='CZIHBVN', ep='CZIHBVN')
    test_one_param_ins('mov %, a', modes, 0xff, a=0x00, result=0x00, p='', ep='')

    # mov x, mem/imm
    modes = [A_IMM, A_DP, A_DP_Y, A_ABS]
    test_one_param_ins('mov x, %', modes, 0x12, x=0x00, ex=0x12, p='CZIHBVN', ep='CIHBV')
    test_one_param_ins('mov x, %', modes, 0x80, ex=0x80, p='', ep='N')
    test_one_param_ins('mov x, %', modes, 0x00, ex=0x00, p='', ep='Z')

    # mov y, mem/imm
    modes = [A_IMM, A_DP, A_DP_X, A_ABS]
    test_one_param_ins('mov y, %', modes, 0x12, y=0x00, ey=0x12, p='CZIHBVN', ep='CIHBV')
    test_one_param_ins('mov y, %', modes, 0x80, ey=0x80, p='', ep='N')
    test_one_param_ins('mov y, %', modes, 0x00, ey=0x00, p='', ep='Z')

    # mov mem, x
    modes = [A_DP, A_DP_Y, A_ABS]
    test_one_param_ins('mov %, x', modes, 0x00, x=0x12, result=0x12, p='CZIHBVN', ep='CZIHBVN')
    test_one_param_ins('mov %, x', modes, 0xff, x=0x00, result=0x00, p='', ep='')

    # mov mem, y
    modes = [A_DP, A_DP_X, A_ABS]
    test_one_param_ins('mov %, y', modes, 0x00, y=0x12, result=0x12, p='CZIHBVN', ep='CZIHBVN')
    test_one_param_ins('mov %, y', modes, 0xff, y=0x00, result=0x00, p='', ep='')

    # mov mem, mem/imm
    modes = [A_DP_DP, A_DP_IMM]
    test_two_param_ins('mov', modes, 0x00, 0x12, 0x12, p='CZIHBVN', ep='CZIHBVN')
    test_two_param_ins('mov', modes, 0xff, 0x00, 0x00, p='', ep='')

    # mov reg, reg
    test('mov a, x', a=0x00, x=0x12, ea=0x12, p='CZIHBVN', ep='CIHBV')
    test('mov a, x', a=0x12, x=0x00, ea=0x00, p='', ep='Z')
    test('mov a, x', a=0x12, x=0x80, ea=0x80, p='', ep='N')
    test('mov a, y', a=0x00, y=0x12, ea=0x12, p='CZIHBVN', ep='CIHBV')
    test('mov a, y', a=0x12, y=0x00, ea=0x00, p='', ep='Z')
    test('mov a, y', a=0x12, y=0x80, ea=0x80, p='', ep='N')
    test('mov x, a', x=0x00, a=0x12, ex=0x12, p='CZIHBVN', ep='CIHBV')
    test('mov x, a', x=0x12, a=0x00, ex=0x00, p='', ep='Z')
    test('mov x, a', x=0x12, a=0x80, ex=0x80, p='', ep='N')
    test('mov y, a', y=0x00, a=0x12, ey=0x12, p='CZIHBVN', ep='CIHBV')
    test('mov y, a', y=0x12, a=0x00, ey=0x00, p='', ep='Z')
    test('mov y, a', y=0x12, a=0x80, ey=0x80, p='', ep='N')
    test('mov x, sp', ex=0xef, p='CZIHBPV', ep='CIHBPVN')

    # mov sp, x
    after = '''\
        mov x, #$EF
        mov sp, x'''
    test('mov sp, x', x=0x00, esp=0x00, p='', ep='', after_checks=after)
    test('mov sp, x', x=0x80, esp=0x80, p='CZIHBPVN', ep='CZIHBPVN', after_checks=after)


def mov1_tests():
    test('mov1 C, $102.2', p='', mem_102=0x04, ep='C')
    test('mov1 C, $102.2', p='C', mem_102=0x04, ep='C')
    test('mov1 C, $102.2', p='CZIHBPVN', mem_102=0xFB, ep='ZIHBPVN')
    test('mov1 C, $102.2', p='ZIHBPVN', mem_102=0xFB, ep='ZIHBPVN')

    test('mov1 $102.3, C', mem_102=0x80, emem_102=0x88, p='C')
    test('mov1 $102.3, C', mem_102=0x88, emem_102=0x88, p='CZIHBPVN')
    test('mov1 $102.3, C', mem_102=0x88, emem_102=0x80, p='')
    test('mov1 $102.3, C', mem_102=0x80, emem_102=0x80, p='ZIHBPVN')


def movw_tests():
    test_16bit_ins('movw ya, %', 0x12AB, eya=0x12AB, p='CZIHBVN', ep='CIHBV')
    test_16bit_ins('movw ya, %', 0x8000, eya=0x8000, p='', ep='N')
    test_16bit_ins('movw ya, %', 0x0000, eya=0x0000, p='', ep='Z')

    test_16bit_ins('movw %, ya', 0x0000, ya=0x1234, result=0x1234, p='CZIHBVN', ep='CZIHBVN')
    test_16bit_ins('movw %, ya', 0xabcd, ya=0x0000, result=0x0000, p='', ep='')


def mul_tests():
    test('mul ya', a=0xAB, y=0xCD, ey=0x88, ea=0xEF, p='CZIHBPVN', ep='CIHBPVN')
    test('mul ya', a=0x05, y=0x02, ey=0x00, ea=0x0A, p='', ep='Z')
    test('mul ya', a=0xFF, y=0xFF, ey=0xFE, ea=0x01, p='', ep='N')
    test('mul ya', a=0x80, y=0x02, ey=0x01, ea=0x00, p='', ep='')


def nop_tests():
    test('nop', p='', ep='')
    test('nop', p='CZIHBPVN', ep='CZIHBPVN')


def not1_tests():
    test('not1 $102.2', mem_102=0x04, emem_102=0x00, p='', ep='')
    test('not1 $102.2', mem_102=0xFF, emem_102=0xFB, p='CZIHBPVN', ep='CZIHBPVN')
    test('not1 $102.2', mem_102=0xC0, emem_102=0xC4, p='', ep='')


def or_tests():
    modes = [A_IMM, A_IND_X_IND_Y, A_IND_X, A_IND_DP_Y, A_DP_X_IND, A_DP, A_DP_X, A_ABS, A_ABS_X, A_ABS_Y, A_DP_DP, A_DP_IMM]
    test_two_param_ins('or', modes, 0x88, 0x19, result=0x99, p='', ep='N')
    test_two_param_ins('or', modes, 0xFF, 0x00, result=0xFF, p='CZIHBVN', ep='CIHBVN')
    test_two_param_ins('or', modes, 0x00, 0x00, result=0x00, p='', ep='Z')


def or1_tests():
    test('or1 C, $102.2', p='C', mem_102=0x04, ep='C')
    test('or1 C, $102.2', p='', mem_102=0x04, ep='C')
    test('or1 C, $102.2', p='CZIHBPVN', mem_102=0xFB, ep='CZIHBPVN')
    test('or1 C, $102.2', p='ZIHBPVN', mem_102=0xFB, ep='ZIHBPVN')
    test('or1 C, /$102.2', p='C', mem_102=0x04, ep='C')
    test('or1 C, /$102.2', p='', mem_102=0x04, ep='')
    test('or1 C, /$102.2', p='CZIHBPVN', mem_102=0xFB, ep='CZIHBPVN')
    test('or1 C, /$102.2', p='ZIHBPVN', mem_102=0xFB, ep='CZIHBPVN')


def pop_tests():
    after = '        push a'
    # The emem_01f0=None will disable the check that memory hasn't changed, because we overwrite it by calling a function
    test('pop a', mem_01f0=0x98, ea=0x98, esp=0xf0, p='', ep='', emem_01f0=None, after_checks=after)
    test('pop a', mem_01f0=0x00, ea=0x00, esp=0xf0, p='CZIHBPVN', ep='CZIHBPVN', emem_01f0=None, after_checks=after)
    test('pop x', mem_01f0=0x98, ex=0x98, esp=0xf0, p='', ep='', emem_01f0=None, after_checks=after)
    test('pop x', mem_01f0=0x00, ex=0x00, esp=0xf0, p='CZIHBPVN', ep='CZIHBPVN', emem_01f0=None, after_checks=after)
    test('pop y', mem_01f0=0x98, ey=0x98, esp=0xf0, p='', ep='', emem_01f0=None, after_checks=after)
    test('pop y', mem_01f0=0x00, ey=0x00, esp=0xf0, p='CZIHBPVN', ep='CZIHBPVN', emem_01f0=None, after_checks=after)
    test('pop psw', mem_01f0=flags(''), p='CZIHBPVN', ep='', emem_01f0=None, after_checks=after)
    test('pop psw', mem_01f0=flags('CZIHBPVN'), p='', ep='CZIHBPVN', emem_01f0=None, after_checks=after)


def push_tests():
    after = '        pop a'
    test('push a', a=0x12, emem_01ef=0x12, esp=0xee, p='', ep='', after_checks=after)
    test('push a', a=0x00, emem_01ef=0x00, esp=0xee, p='CZIHBPVN', ep='CZIHBPVN', after_checks=after)
    test('push x', x=0x12, emem_01ef=0x12, esp=0xee, p='', ep='', after_checks=after)
    test('push x', x=0x00, emem_01ef=0x00, esp=0xee, p='CZIHBPVN', ep='CZIHBPVN', after_checks=after)
    test('push y', y=0x12, emem_01ef=0x12, esp=0xee, p='', ep='', after_checks=after)
    test('push y', y=0x00, emem_01ef=0x00, esp=0xee, p='CZIHBPVN', ep='CZIHBPVN', after_checks=after)
    test('push psw', p='CZIHBPVN', emem_01ef=flags('CZIHBPVN'), esp=0xee, ep='CZIHBPVN', after_checks=after)
    test('push psw', p='', emem_01ef=flags(''), esp=0xee, ep='', after_checks=after)


def ret_tests():
    before = '''\
        mov a, #(.ok >> 8)
        push a
        mov a, #(.ok & $FF)
        push a'''
    after = '''\
        call save_results
        bra .to_fail
    .ok:'''
    test('ret', esp=0xef, p='', ep='', before_regs=before, after_ins=after)
    test('ret', esp=0xef, p='CZIHBPVN', ep='CZIHBPVN', before_regs=before, after_ins=after)


def ret1_tests():
    before = '''\
        mov a, #(.ok >> 8)
        push a
        mov a, #(.ok & $FF)
        push a
        push a'''  # last push will be overwritten
    after = '''\
        call save_results
        bra .to_fail
    .ok:'''
    test('ret1', esp=0xef, mem_1ed=flags(''), p='CZIHBPVN', ep='', emem_1ed=None, before_regs=before, after_ins=after)
    test('ret1', esp=0xef, mem_1ed=flags('CZIHBPVN'), p='', ep='CZIHBPVN', emem_1ed=None, before_regs=before, after_ins=after)


def rol_tests():
    modes = [A_IMPLIED, A_DP, A_DP_X, A_ABS]
    test_one_param_ins('rol', modes, 0xFF, 0xFE, p='', ep='NC')
    test_one_param_ins('rol', modes, 0xFF, 0xFF, p='C', ep='NC')
    test_one_param_ins('rol', modes, 0x80, 0x00, p='', ep='ZC')
    test_one_param_ins('rol', modes, 0x03, 0x07, p='CZIHBVN', ep='IHBV')


def ror_tests():
    modes = [A_IMPLIED, A_DP, A_DP_X, A_ABS]
    test_one_param_ins('ror', modes, 0xFF, 0x7F, p='', ep='C')
    test_one_param_ins('ror', modes, 0xFF, 0xFF, p='C', ep='NC')
    test_one_param_ins('ror', modes, 0x01, 0x00, p='', ep='ZC')
    test_one_param_ins('ror', modes, 0x06, 0x83, p='CZIHBVN', ep='IHBVN')


def sbc_tests():
    modes = [A_IMM, A_IND_X_IND_Y, A_IND_X, A_IND_DP_Y, A_DP_X_IND, A_DP, A_DP_X, A_ABS, A_ABS_X, A_ABS_Y, A_DP_DP, A_DP_IMM]
    test_two_param_ins('sbc', modes, 0x46, 0x12, p='C', result=0x34, ep='CH')
    test_two_param_ins('sbc', modes, 0x47, 0x12, p='ZIHBVN', result=0x34, ep='CHIB')
    test_two_param_ins('sbc', modes, 0xCB, 0xCA, p='', result=0x00, ep='ZCH')
    test_two_param_ins('sbc', modes, 0x90, 0x21, p='C', result=0x6F, ep='CV')
    test_two_param_ins('sbc', modes, 0x70, 0xE0, p='H', result=0x8F, ep='VN')
    test_two_param_ins('sbc', modes, 0x00, 0x00, p='', result=0xFF, ep='N')


def set1_tests():
    for bit in range(8):
        test_one_param_ins(f'set1 %.{bit}', [A_DP], 0xff, 0xff, p='', ep='')
        test_one_param_ins(f'set1 %.{bit}', [A_DP], 0x00, 1 << bit, p='', ep='')
        test_one_param_ins(f'set1 %.{bit}', [A_DP], 0xff, 0xff, p='CZIHBVN', ep='CZIHBVN')
        test_one_param_ins(f'set1 %.{bit}', [A_DP], 0x12, 0x12 | (1 << bit), p='CZIHBVN', ep='CZIHBVN')


def subw_tests():
    def test_subw(val1, val2, p, ep):
        res = (val1 - val2) & 0xFFFF
        test_16bit_ins('subw ya, %', val2, ya=val1, p=p, eya=res, ep=ep)

    test_subw(0x3412, 0x00ff, p='', ep='CH')
    test_subw(0x3412, 0x00ff, p='CZIHBVN', ep='IBCH')
    test_subw(0x0000, 0x0001, p='', ep='N')
    test_subw(0xABCD, 0xABCD, p='', ep='CHZ')
    test_subw(0x1234, 0x0300, p='H', ep='C')
    test_subw(0x1234, 0x0034, p='', ep='CH')
    test_subw(0x1234, 0x1200, p='', ep='CH')
    test_subw(0x8000, 0x0001, p='', ep='CV')
    test_subw(0x7123, 0xEFED, p='', ep='VN')


def tclr1_tests():
    test_one_param_ins('tclr1', [A_ABS], 0xFF, a=0x09, result=0xF6, p='', ep='')
    test_one_param_ins('tclr1', [A_ABS], 0x05, a=0x15, result=0x00, p='CZIHBPVN', ep='CIHBPV')
    test_one_param_ins('tclr1', [A_ABS], 0x12, a=0x12, result=0x00, p='', ep='Z')
    test_one_param_ins('tclr1', [A_ABS], 0x12, a=0x02, result=0x10, p='', ep='N')


def tset1_tests():
    test_one_param_ins('tset1', [A_ABS], 0x11, a=0x09, result=0x19, p='CZIHBPVN', ep='CIHBPVN')
    test_one_param_ins('tset1', [A_ABS], 0x09, a=0x11, result=0x19, p='CZIHBPVN', ep='CIHBPV')
    test_one_param_ins('tset1', [A_ABS], 0x05, a=0x05, result=0x05, p='CZIHBPVN', ep='CZIHBPV')
    test_one_param_ins('tset1', [A_ABS], 0xFF, a=0xFE, result=0xFF, p='', ep='N')


def xcn_tests():
    test('xcn a', a=0x12, ea=0x21, p='CZIHBPVN', ep='CIHBPV')
    test('xcn a', a=0x18, ea=0x81, p='', ep='N')
    test('xcn a', a=0x00, ea=0x00, p='', ep='Z')
    test('xcn a', a=0xF0, ea=0x0F, p='', ep='')


def add_all_tests():
    adc_tests()
    addw_tests()
    and_tests()
    and1_tests()
    asl_tests()
    bbc_tests()
    bbs_tests()
    branch_tests()
    brk_tests()
    call_tests()  # also PCALL and TCALL
    cbne_tests()
    clr1_tests()
    flag_manip_tests()
    cmp_tests()
    cmpw_tests()
    dbnz_tests()
    dec_tests()
    decw_tests()
    eor_tests()
    eor1_tests()
    inc_tests()
    incw_tests()
    jmp_tests()
    lsr_tests()
    mov_tests()
    mov1_tests()
    movw_tests()
    mul_tests()
    nop_tests()
    not1_tests()
    or1_tests()
    pop_tests()
    push_tests()
    ret_tests()
    ret1_tests()
    rol_tests()
    ror_tests()
    sbc_tests()
    set1_tests()
    subw_tests()
    tclr1_tests()
    tset1_tests()
    xcn_tests()

    # Leave daa/das/div to the end because some of their behavior is not officially documented
    daa_tests()
    das_tests()
    div_tests()


def start_spc_file():
    spc_code.clear()
    spc_code.append('; Auto-generated by make_tests.py\n')
    spc_code.append('include "spc_common.inc"')
    spc_code.append('start_tests:\n')


def close_spc_file():
    global file_num

    spc_code.append('''        jmp success''')
    with open(f'spc_tests{file_num}.asm', 'w') as f:
        f.write('\n'.join(spc_code) + '\n')
    file_num += 1


def main():
    tests_txt.append('Auto-generated by make_tests.py\n')

    start_spc_file()
    add_all_tests()

    with open('tests.txt', 'w') as f:
        f.write('\n'.join(tests_txt) + '\n')
    close_spc_file()


main()
