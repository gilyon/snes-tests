from textwrap import dedent
import re

test_num = 0
bank_num = 0
tests_in_bank = 0
asm_code = []
tests_txt = []

# Limit tests per bank to not exceed 32KB. Bank 0 has less room due to other code and data in it.
TESTS_IN_BANK0 = 230
TESTS_PER_BANK = 360


# emulation mode flag (E) is treated as a 9th bit here for convenience
FLAGS_MAP = {'C': 1, 'Z': 2, 'I': 4, 'D': 8, 'X': 0x10, 'B': 0x10, 'M': 0x20, 'V': 0x40, 'N': 0x80, 'E': 0x100}

FLAG_M = 0x20
FLAG_X = 0x10
FLAG_E = 0x100  # not a real flag

OPCODE_STP = 0xDB

# Addressing modes
A_IMM = 0           # #$12 or #$1234
A_ACCUM = 1         # a
A_ABS = 2           # $1234
A_ABS_X = 3         # $1234,x
A_ABS_Y = 4         # $1234,y
A_DIR = 5           # $12
A_DIR_X = 6         # $12,x
A_DIR_Y = 7         # $12,y
A_IND_DIR = 8      # ($12)
A_IND_LONG_DIR = 9  # [$12]
A_DIR_X_IND = 10    # ($12,x)
A_IND_DIR_Y = 11    # ($12),y
A_IND_LONG_DIR_Y = 12  # [$12],y
A_LONG = 13         # $123456
A_LONG_X = 14       # $123456,x
A_STACK_S = 15      # $12,s
A_IND_STACK_S_Y = 16  # ($12,s),y

A_IND_ABS = 17      # ($1234) - jmp only
A_ABS_X_IND = 18    # ($1234,x) - jmp/jsr only
A_IND_LONG_ABS = 19  # [$1234] - jmp (a.k.a. jml) only

# Others for specific instructions: implied,  rel8,  rel16,  source+dest,


# Modes for read-only instructions like lda or eor
READ_MODES = [A_IMM, A_DIR_X_IND, A_STACK_S, A_DIR, A_IND_LONG_DIR, A_ABS, A_LONG, A_IND_DIR_Y,
              A_IND_DIR, A_IND_STACK_S_Y, A_DIR_X, A_IND_LONG_DIR_Y, A_ABS_Y, A_ABS_X, A_LONG_X]


# Instructions whose operand size depends on the X flag
_ins_index_size = set(('cpx cpy dex dey inx iny ldx ldy phx phy plx ply stx sty tax tay tsx txy tyx'.split()))
# Instructions whose operand size depends on the M flag
_ins_acc_size = set(('adc and asl bit cmp dec eor inc lda lsr ora pha pla rol ror sbc sta stz trb tsb txa tya'.split()))

_re_leading_space = re.compile(r'^[ \t]+.', re.MULTILINE)

UNDOCUMENTED_COMMENT = 'This tests undocumented behavior - see the cputest README file for details'


def add_asm(code):
    code = dedent(code)
    if not _re_leading_space.search(code):
        # all lines are unindented, so add indentation
        code = '    ' + code.replace('\n', '\n    ')

    asm_code.append(code)


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


def next_bank_if_needed():
    if (bank_num == 0 and tests_in_bank == TESTS_IN_BANK0) or (bank_num != 0 and tests_in_bank == TESTS_PER_BANK):
        next_bank()


def test(ins, a=0x1234, x=0x3456, y=0x5678, p=None, ea=None, ex=None, ey=None, ep=None,
         dbr=None, edbr=None, d=None, ed=None, s=None, es=None,
         before_regs=None, before_ins=None, after_ins=None, after_checks=None,
         ins_name=None, comment=None, **kwargs):
    '''Test the given instruction, setting registers/memory before and checking them after.

    - ins is the instruction to execute
    - a, x, y, p, s, d, dbr are the input register values
    - e* are the expected output values. If None then they should be the same as input (for a,x,y).
    - mem_abcdef means this is the input value in memory location $ABCDEF.
    - emem_abcdef is the expected output value in this memory location.
    - before_regs, before_ins, after_ins, after_checks are used to add additional assembly code
      at specific points.
    - ins_name will be written instead of the ins in tests.txt if provided. It doesn't
      affect the ROM.
    - comment wil be written to texts.txt.
    '''

    global test_num, tests_in_bank
    next_bank_if_needed()

    p = flags(p)
    ep = flags(ep, p)
    if ea is None:
        ea = a
    if ex is None:
        ex = x
        if (p | ep) & (FLAG_X | FLAG_E):
            ex &= 0xFF
    if ey is None:
        ey = y
        if (p | ep) & (FLAG_X | FLAG_E):
            ey &= 0xFF

    mem_input = []
    mem_output = {}
    for (k, v) in kwargs.items():
        if k.startswith('mem_'):
            mem_input.append((int(k[4:], 16), v))
        elif k.startswith('memw_'):
            addr = int(k[5:], 16)
            mem_input.append((addr, v & 0xFF))
            mem_input.append((addr + 1, v >> 8))
        elif k.startswith('emem_'):
            mem_output[int(k[5:], 16)] = v
        elif k.startswith('ememw_'):
            addr = int(k[6:], 16)
            mem_output[addr] = v & 0xFF
            mem_output[addr + 1] = v >> 8
        else:
            assert False, 'Invalid keyword: ' + k

    # assumption: 16-bit indexes, 8-bit accum
    if comment is not None:
        asm_code.append(f'; {comment}')
    add_asm(f'''\
        test{test_num:04x}:
        .export test{test_num:04x}: far
            ldx #${test_num:02x}
            jsl init_test''')

    if before_regs:
        add_asm(before_regs)
    for loc, val in mem_input:
        # if loc not in mem_output:
        #    mem_output[loc] = val
        add_asm(f'lda #${val:02x}')
        add_asm(f'sta ${loc:06x}')

    if dbr is not None:
        add_asm(f'''\
            lda #${dbr:02x}
            pha
            plb''')
    add_asm('''\
        rep #$20
        .a16''')
    if d:
        add_asm(f'''\
            lda #${d:04x}
            tcd''')
    if s is not None:
        add_asm(f'''\
            ldx #${s:04x}
            txs''')
    add_asm(f'''\
        lda #${a:04x}
        ldx #${x:04x}
        ldy #${y:04x}''')
    if p & FLAG_E:
        add_asm('sec')
        add_asm('xce')
    add_asm(f'''\
        rep #${~p & 0xFF:02x}
        sep #${p & 0xFF:02x}''')
    if p & (FLAG_E | FLAG_M):
        add_asm('.a8')
    if p & (FLAG_E | FLAG_X):
        add_asm('.i8')

    if before_ins:
        add_asm(before_ins)
    add_asm(ins)
    if after_ins:
        add_asm(after_ins)
    add_asm(f'''\
        jsr bank{bank_num}_save_results ; now X=0, E=0, M=1
        .a8
        .i16
        cpx #${ex:04x}
        bne @to_fail
        cpy #${ey:04x}
        bne @to_fail
        ldx result_p
        cpx #${ep:04x}
        bne @to_fail
        ldx result_a
        cpx #${ea:04x}
        bne @to_fail''')
    if es is not None:
        add_asm(f'''\
            ldx result_s
            cpx #${es:04x}
            bne @to_fail''')
    if edbr is not None:
        add_asm(f'''\
            lda result_dbr
            cmp #${edbr:02x}
            bne @to_fail''')
    if ed is not None:
        add_asm(f'''\
            ldx result_d
            cpx #${ed:04x}
            bne @to_fail''')

    for loc, val in mem_output.items():
        if val is not None:
            add_asm(f'''\
                lda ${loc:06x}
                cmp #${val:02x}
                bne @to_fail''')

    if es is not None:
        # We expect S to change, so restore the original value now
        add_asm('''\
            ldx #$1EF
            txs''')
    if after_checks:
        add_asm(after_checks)

    add_asm('''\
        bra @next_test
    @to_fail:
        jml fail
    @next_test:
    ''')

    tests_txt.append(f'Test {test_num:04x}: {ins_name or ins}')
    inputs = f'A=${a:04x} X=${x:04x} Y=${y:04x} P=${p & 0xFF:02x} E={1 if p & FLAG_E else 0}'
    if s is not None:
        inputs += f' S=${s:04x}'
    if dbr is not None:
        inputs += f' DBR=${dbr:02x}'
    if d is not None:
        inputs += f' D=${d:04x}'
    if mem_input:
        inputs += ' ' + ' '.join(f'(${loc:06x})=${val:02x}' for loc, val in mem_input)
    outputs = f'A=${ea:04x} X=${ex:04x} Y=${ey:04x} P=${ep & 0xFF:02x} E={1 if ep & FLAG_E else 0}'
    if es is not None:
        outputs += f' S={es:04x}'
    if edbr is not None:
        outputs += f' DBR={edbr:02x}'
    if ed is not None:
        outputs += f' D={ed:04x}'
    if mem_output:
        outputs += ' ' + ' '.join(f'(${loc:06x})=${val:02x}' for loc, val in mem_output.items() if val is not None)

    tests_txt.append(f'   Input: {inputs}')
    tests_txt.append(f'   Expected output: {outputs}')
    if before_regs or before_ins or after_ins or after_checks:
        tests_txt.append('   Additional initialization or checks are performed - see assembly')
    if comment is not None:
        tests_txt.append(f'   Note: {comment}')

    test_num += 1
    tests_in_bank += 1


def test_ins_with_modes(ins, modes, val, result=None, p='', ep=None, **kwargs):
    ''' Test an instruction in all the specified modes.
    val is the input value of memory/immediate for the operand. result (if given) is the expected output value.
    Other parameters are passed to test().

    Each instruction should be tested with all supported modes at least once for 16-bit and once for 8-bit.
    Additional tests can be performed on less modes to save space.
    '''
    def do_test(operand, add_flag='', mem=None, **kwargs2):
        full_ins = ins + ' ' + operand
        kwargs3 = kwargs.copy()
        kwargs3.update(kwargs2)
        if mem is not None:
            suffix = '' if bits == 8 else 'w'
            kwargs3[f'mem{suffix}_{mem:06x}'] = val
            kwargs3[f'emem{suffix}_{mem:06x}'] = result
        p2 = flags(p, add=add_flag)
        ep2 = flags(ep, p, add=add_flag)
        if flags(add_flag) & FLAG_E:
            ep2 |= FLAG_M | FLAG_X  # these are always on in emulation mode
        test(full_ins, p=p2, ep=ep2, **kwargs3)

    if result is None:
        result = val

    p = flags(p)
    ep = flags(ep, p)

    operand_uses_x_size = ins in _ins_index_size
    operand_uses_m_size = ins in _ins_acc_size
    if operand_uses_x_size:
        bits = 8 if p & FLAG_X else 16
    elif operand_uses_m_size:
        bits = 8 if p & FLAG_M else 16
    else:
        assert False, 'This function is only called on instructions with operands of variable size'

    for mode in modes:
        if mode == A_IMM:
            assert result == val
            do_test(f'#${val:02x}')  # may be 8-bit or 16-bit, depending on flags
        elif mode == A_ACCUM:
            if bits == 16:
                do_test('a', a=val, ea=result)
            else:  # 8-bit: add some data in high bits to see it doesn't change
                do_test('a', a=val | 0x1200, ea=result | 0x1200)
        elif mode == A_ABS:
            do_test('$FFFF', dbr=0x7E, mem=0x7effff)
            if bits == 8:
                do_test('$FFFF', dbr=0x7E, mem=0x7effff, add_flag='E')  # works the same in emulation mode
        elif mode == A_ABS_X:
            if not operand_uses_x_size or bits == 16:
                do_test('$FFFF,x', x=0x300, dbr=0x7E, mem=0x7f02ff)  # 16 bit index
            if not operand_uses_x_size or bits == 8:
                do_test('$FFFF,x', x=0x1230, dbr=0x7E, mem=0x7f002f, add_flag='X')  # 8 bit index
            if bits == 8:
                do_test('$FFFF,x', x=0x1230, dbr=0x7E, mem=0x7f002f, add_flag='E')  # emulation mode (8 bit index)
        elif mode == A_ABS_Y:
            if not operand_uses_x_size or bits == 16:
                do_test('$FFFF,y', y=0x300, dbr=0x7E, mem=0x7f02ff)  # 16 bit index
            if not operand_uses_x_size or bits == 8:
                do_test('$FFFF,y', y=0x1230, dbr=0x7E, mem=0x7f002f, add_flag='X')  # 8 bit index
            if bits == 8:
                do_test('$FFFF,y', y=0x1230, dbr=0x7E, mem=0x7f002f, add_flag='E')  # emulation mode (8 bit index)
        elif mode == A_DIR:
            # set DBR to 7F to make sure the actual read is done from bank 0 and not DBR.
            do_test('$34', mem=0x33, d=0xffff, dbr=0x7F)
            if bits == 8:
                # Note: although supposedly E=1 and DL=0 wraps within the page, wrapping will never happen for DL=0 so this is meaningless.
                do_test('$34', mem=0x33, d=0xffff, dbr=0x7F, add_flag='E')
        elif mode == A_DIR_X:
            if not operand_uses_x_size or bits == 16:
                do_test('$02,x', x=0x133, mem=0x134, d=0xffff, dbr=0x7F)  # 16 bit index
            if not operand_uses_x_size or bits == 8:
                do_test('$02,x', x=0x1232, mem=0x33, d=0xffff, dbr=0x7F, add_flag='X')  # 8 bit index
            if bits == 8:
                # E=1:  Wraps in page for DL=0, or bank for DL!=0
                do_test('$02,x', x=0x32, mem=0x33, d=0xffff, dbr=0x7F, add_flag='E')
                do_test('$FF,x', x=0x34, mem=0x133, d=0x0100, dbr=0x7F, add_flag='E')
        elif mode == A_DIR_Y:
            assert operand_uses_x_size  # only ldx, stx
            if bits == 16:
                do_test('$02,y', y=0x133, mem=0x134, d=0xffff, dbr=0x7F)  # 16 bit index
            if bits == 8:
                do_test('$02,y', y=0x1232, mem=0x33, d=0xffff, dbr=0x7F, add_flag='X')  # 8 bit index
                # E=1:  Wraps in page for DL=0, or bank for DL!=0
                do_test('$02,y', y=0x32, mem=0x33, d=0xffff, dbr=0x7F, add_flag='E')
                do_test('$FF,y', y=0x34, mem=0x133, d=0x0100, dbr=0x7F, add_flag='E')
        elif mode == A_IND_DIR:
            do_test('($34)', mem=0x7F1234, d=0xffff, memw_0033=0x1234, dbr=0x7F)
            # There doesn't seem to be a way to test bank 0 wrapping due to wram mirroring
            if bits == 16:  # test no bank wrapping at destination
                do_test('($34)', mem=0x7EFFFF, d=0xffff, memw_0033=0xFFFF, dbr=0x7E)
            if bits == 8:
                # E=1, DL=0: Wraps in page
                do_test('($FF)', mem=0x7F1234, d=0x0100, mem_01FF=0x34, mem_0100=0x12, dbr=0x7F, add_flag='E')
                # No page wrap with DL != 0. The write at 100 is to make sure we don't use it:
                do_test('($FE)', mem=0x7F1234, d=0x0101, memw_01FF=0x1234, mem_0100=0x00, dbr=0x7F, add_flag='E')
        elif mode == A_IND_LONG_DIR:
            do_test('[$34]', mem=0x7F1234, d=0xffff, memw_0033=0x1234, mem_0035=0x7F)
            # There doesn't seem to be a way to test bank 0 wrapping due to wram mirroring
            if bits == 16:  # test no bank wrapping at destination
                do_test('[$34]', mem=0x7EFFFF, d=0xffff, memw_0033=0xFFFF, mem_0035=0x7E)
            if bits == 8:
                # E=1: No wrapping in page, even with DL=0
                do_test('[$FF]', mem=0x7F1234, d=0x0100, memw_01FF=0x1234, mem_0201=0x7F, add_flag='E')
        elif mode == A_DIR_X_IND:
            assert operand_uses_m_size
            # $00FFA0 contains a pointer to $1212. Use it to test bank 0 wrapping
            do_test('($10,x)', mem=0x7F1212, d=0xffff, x=0xFF91, dbr=0x7F)
            if bits == 16:  # test no bank wrapping at destination
                do_test('($10,x)', mem=0x7EFFFF, d=0xff00, x=0x123, memw_0033=0xFFFF, dbr=0x7E)

            # 8 bit index
            do_test('($90,x)', mem=0x7F1212, x=0x1210, d=0xff00, dbr=0x7F, add_flag='X')

            if bits == 8:
                # E=1, DL=0: Wraps in page
                do_test('($EF,x)', mem=0x7F1234, x=0x10, d=0x0100, mem_01FF=0x34, mem_0100=0x12, dbr=0x7F, add_flag='E')
                do_test('($F0,x)', mem=0x7F1234, x=0x10, d=0x0100, memw_0100=0x1234, dbr=0x7F, add_flag='E')

                # With E=1, DL != 0: There is no wrapping for the dp+D+X calculation, but the +1 addition for the high byte DOES wrap.
                # This behavior is undocumented.
                do_test('($F6,x)', mem=0x7F1234, x=0xEE, d=0x011A, memw_02FE=0x1234, dbr=0x7F, add_flag='E')
                do_test('($F7,x)', mem=0x7F1234, x=0xEE, d=0x011A, mem_02FF=0x34, mem_0200=0x12, dbr=0x7F, add_flag='E',
                        comment=UNDOCUMENTED_COMMENT)
        elif mode == A_IND_DIR_Y:
            assert operand_uses_m_size
            do_test('($34),y', mem=0x7F0FDC, y=0x1100, d=0xffff, memw_0033=0xFEDC, dbr=0x7E)
            # 8 bit index
            do_test('($34),y', mem=0x7FFEEC, y=0x1110, d=0xffff, memw_0033=0xFEDC, dbr=0x7F, add_flag='X')
            # There doesn't seem to be a way to test bank 0 wrapping due to wram mirroring
            if bits == 16:  # test no bank wrapping at destination
                do_test('($34),y', mem=0x7EFFFF, d=0xffff, y=0x1111, memw_0033=0xEEEE, dbr=0x7E)
            if bits == 8:
                # E=1, DL=0: Wraps in page
                do_test('($FF),y', mem=0x7F1244, y=0x10, d=0x0100, mem_01FF=0x34, mem_0100=0x12, dbr=0x7F, add_flag='E')
                # No page wrap with DL != 0. The write at 100 is to make sure we don't use it:
                do_test('($FE),y', mem=0x7F1244, y=0x10, d=0x0101, memw_01FF=0x1234, mem_0100=0x00, dbr=0x7F, add_flag='E')
        elif mode == A_IND_LONG_DIR_Y:
            assert operand_uses_m_size
            do_test('[$34],y', mem=0x7F0FDC, y=0x1100, d=0xffff, memw_0033=0xFEDC, mem_0035=0x7E)
            # There doesn't seem to be a way to test bank 0 wrapping due to wram mirroring

            # 8 bit index
            do_test('[$34],y', mem=0x7FFEEC, y=0x1110, d=0xffff, memw_0033=0xFEDC, mem_0035=0x7F, add_flag='X')
            if bits == 16:  # test no bank wrapping at destination
                do_test('[$34],y', mem=0x7EFFFF, d=0xffff, y=0x1111, memw_0033=0xEEEE, mem_0035=0x7E)
            if bits == 8:
                # E=1: No wrapping in page, even with DL=0
                do_test('[$FF],y', mem=0x7F1244, y=0x10, d=0x0100, memw_01FF=0x1234, mem_0201=0x7F, add_flag='E')
        elif mode == A_LONG:
            do_test('$7EFFFF', mem=0x7EFFFF)
            if bits == 8:
                do_test('$7EFFFF', mem=0x7effff, add_flag='E')  # works the same in emulation mode
        elif mode == A_LONG_X:
            assert operand_uses_m_size
            do_test('$7EFFFF,x', x=0x300, mem=0x7f02ff)  # 16 bit index
            do_test('$7EFFFF,x', x=0x1230, mem=0x7f002f, add_flag='X')  # 8 bit index
            if bits == 8:
                do_test('$7EFFFF,x', x=0x1230, mem=0x7f002f, add_flag='E')  # emulation mode (8 bit index)
        elif mode == A_STACK_S:
            # currently s = 1ef
            do_test('$12,s', mem=0x201)
            if bits == 8:
                # E=1: Can leave the stack page.
                do_test('$12,s', mem=0x201, add_flag='E')
        elif mode == A_IND_STACK_S_Y:
            # currently s = 1ef
            assert operand_uses_m_size
            do_test('($10,s),y', mem=0x7F0FDC, y=0x1100, memw_01FF=0xFEDC, dbr=0x7E)
            # 8 bit index
            do_test('($10,s),y', mem=0x7FFEEC, y=0x1110, memw_01FF=0xFEDC, dbr=0x7F, add_flag='X')
            # There doesn't seem to be a way to test bank 0 wrapping due to wram mirroring
            if bits == 16:  # test no bank wrapping at destination
                do_test('($10,s),y', mem=0x7EFFFF, y=0x1111, memw_01FF=0xEEEE, dbr=0x7E)
            if bits == 8:
                # E=1: Can leave the stack page.
                do_test('($12,s),y', mem=0x7FFEEC, y=0x1110, memw_0201=0xFEDC, dbr=0x7F, add_flag='E')


def jml_to_label_instructions(addr, label):
    '''Return asm code that writes a jmp long (jml) instruction to the specified label at the specified address'''
    return f'''\
        lda #$5C  ; jmp long opcode
        sta ${addr:04x}
        lda #<{label}
        sta ${addr+1:04x}
        lda #>{label}
        sta ${addr+2:04x}
        lda #^{label}
        sta ${addr+3:04x}'''


def jml_to_ok_instructions(addr):
    '''Return asm code that writes a jmp long (jml) instruction to the @ok label at the specified address'''
    return jml_to_label_instructions(addr, '@ok')


def adc_binary_tests():
    test_ins_with_modes('adc', READ_MODES, a=0x1234, val=0xEDCB, ea=0, p='C', ep='CZ')
    test_ins_with_modes('adc', [A_IMM], a=0x6789, val=0x2000, ea=0x8789, p='', ep='VN')
    test_ins_with_modes('adc', [A_IMM], a=0x8000, val=0x8000, ea=0, p='ZIVN', ep='CZIV')
    test_ins_with_modes('adc', [A_IMM], a=0x9000, val=0xE000, ea=0x7001, p='C', ep='CV')

    test_ins_with_modes('adc', READ_MODES, a=0x1112, val=0xED, ea=0x1100, p='MC', ep='MCZ')
    test_ins_with_modes('adc', [A_IMM], a=0x1167, val=0x20, ea=0x1187, p='M', ep='MVN')
    test_ins_with_modes('adc', [A_IMM], a=0x1180, val=0x80, ea=0x1100, p='MZIVN', ep='MCZIV')
    test_ins_with_modes('adc', [A_IMM], a=0x1190, val=0xE0, ea=0x1171, p='MC', ep='MCV')


def adc_decimal_tests():
    # The V flag is binary overflow for the sum of the most significant nibbles + carry from lower nibbles.
    test_ins_with_modes('adc', READ_MODES, a=0x1234, val=0x8765, ea=0, p='DC', ep='DCZ')
    test_ins_with_modes('adc', [A_IMM], a=0x3550, val=0x4470, ea=0x8020, p='D', ep='DNV')
    test_ins_with_modes('adc', [A_IMM], a=0x4000, val=0x3999, ea=0x7999, p='D', ep='D')
    test_ins_with_modes('adc', [A_IMM], a=0xDCBA, val=0xDBCA, ea=0x1EEA, p='D', ep='DC')

    test_ins_with_modes('adc', READ_MODES, a=0xCC12, val=0x87, ea=0xCC00, p='DMC', ep='DMCZ')
    test_ins_with_modes('adc', [A_IMM], a=0xCC40, val=0x40, ea=0xCC80, p='DM', ep='DMNV')
    test_ins_with_modes('adc', [A_IMM], a=0xCC40, val=0x39, ea=0xCC79, p='DM', ep='DM')
    test_ins_with_modes('adc', [A_IMM], a=0xCCDC, val=0xDB, ea=0xCC1D, p='DM', ep='DMC')


def and_tests():
    test_ins_with_modes('and', READ_MODES, a=0xFEFF, val=0xEF5C, p='', ea=0xEE5C, ep='N')
    test_ins_with_modes('and', [A_IMM], a=0x5555, val=0xAAAA, p='', ea=0x0000, ep='Z')
    test_ins_with_modes('and', [A_IMM], a=0x1234, val=0x4300, p='CZIDVN', ea=0x0200, ep='CIDV')

    test_ins_with_modes('and', READ_MODES, a=0x12FE, val=0xDF, p='M', ea=0x12DE, ep='MN')
    test_ins_with_modes('and', [A_IMM], a=0x5555, val=0xAA, p='M', ea=0x5500, ep='MZ')


def asl_tests():
    modes = [A_ACCUM, A_DIR, A_ABS, A_DIR_X, A_ABS_X]
    test_ins_with_modes('asl', modes, 0x8000, 0x0000, p='', ep='ZC')
    test_ins_with_modes('asl', modes, 0x4111, 0x8222, p='CZIDVN', ep='IDVN')

    test_ins_with_modes('asl', modes, 0x80, 0x00, p='M', ep='MZC')
    test_ins_with_modes('asl', modes, 0x41, 0x82, p='CZIDMVN', ep='IDMVN')


def bit_tests():
    # immediate mode doesn't set N and V flags
    test_ins_with_modes('bit', [A_IMM], 0x1234, a=0x9377, p='CZIDVN', ep='CIDVN')
    test_ins_with_modes('bit', [A_IMM], 0xAAAA, a=0x5555, p='', ep='Z')
    test_ins_with_modes('bit', [A_IMM], 0xAA, a=0x56, p='M', ep='M')
    test_ins_with_modes('bit', [A_IMM], 0xAA, a=0x55, p='M', ep='MZ')

    modes = [A_DIR, A_ABS, A_DIR_X, A_ABS_X]
    test_ins_with_modes('bit', modes, 0x5234, a=0x9377, p='CZIDVN', ep='CIDV')
    test_ins_with_modes('bit', modes, 0xAAAA, a=0x5555, p='', ep='NZ')
    test_ins_with_modes('bit', modes, 0x52, a=0x93, p='M', ep='MV')
    test_ins_with_modes('bit', modes, 0xAA, a=0x55, p='M', ep='MZN')


def branch_tests():
    def test_branch_taken(ins, opcode, p):
        # branch forward: 7effc0 = B?? +127.  7e0041 = jml @ok.  7effc2 = STP
        before = jml_to_ok_instructions(0x7e0041)
        after = f'''
            jsr bank{bank_num}_save_results
            bra @to_fail
        @ok:'''
        test('jml $7EFFC0', ins_name=f'{ins} +127', before_regs=before, after_ins=after,
             mem_7effc0=opcode, mem_7effc1=127, mem_7effc2=OPCODE_STP, mem_7f0041=OPCODE_STP, p=p)

        # branch backwards: 7f0040 = B?? -128.  7fffc2 = jml @ok.  7f0042 = STP
        before = jml_to_ok_instructions(0x7fffc2)
        after = '@ok:'
        test('jml $7F0040', ins_name=f'{ins} -128', before_regs=before, after_ins=after,
             mem_7f0040=opcode, mem_7f0041=0x80, mem_7f0042=OPCODE_STP, mem_7effc2=OPCODE_STP, p=p)

    def test_branch_not_taken(ins, p):
        after = f'''\
            bra @ok
        @not_ok:
            jsr bank{bank_num}_save_results
            bra @to_fail
        @ok:
        '''
        test(f'{ins} @not_ok', after_ins=after, p=p)

    test_branch_taken('bra', 0x80, p='')

    test_branch_taken('bcc', 0x90, p='')
    test_branch_not_taken('bcc', p='C')
    test_branch_taken('bcs', 0xB0, p='CZIDXMVNE')
    test_branch_not_taken('bcs', p='')

    test_branch_taken('bne', 0xD0, p='')
    test_branch_not_taken('bne', p='Z')
    test_branch_taken('beq', 0xF0, p='Z')
    test_branch_not_taken('beq', p='')

    test_branch_taken('bpl', 0x10, p='')
    test_branch_not_taken('bpl', p='N')
    test_branch_taken('bmi', 0x30, p='N')
    test_branch_not_taken('bmi', p='')

    test_branch_taken('bvc', 0x50, p='')
    test_branch_not_taken('bvc', p='V')
    test_branch_taken('bvs', 0x70, p='V')
    test_branch_not_taken('bvs', p='')


def brk_cop_tests():
    after = '@ok:'

    # Addresses 1000,1004,1008,100C are the BRK/COP handler addresses
    for ins_name, opcode, native_addr, emul_addr in [('brk', 0x00, 0x1000, 0x1008), ('cop #$DB', 0x02, 0x1004, 0x100C)]:
        before = jml_to_ok_instructions(native_addr)
        after_checks = f'''\
            lda #${OPCODE_STP:02x}
            sta ${native_addr:02x}'''  # stop on further calls
        test('jml $7e8000', ins_name=ins_name, mem_7e8000=opcode, mem_7e8001=OPCODE_STP, p='DCZ', ep='ICZ',
             es=0x1eb, emem_1ef=0x7e, ememw_1ed=0x8002, emem_1ec=flags('DCZ'),
             before_regs=before, after_ins=after, after_checks=after_checks)

        before = jml_to_ok_instructions(emul_addr)
        after_checks = f'''\
            lda #${OPCODE_STP:02x}
            sta ${emul_addr:02x}'''  # stop on further calls
        # E=1: Don't push pbr, stack writes wrap in page 1
        test('jml $7e8000', ins_name=ins_name, mem_7e8000=opcode, mem_7e8001=OPCODE_STP, p='DCZE', ep='ICZMXE',
             s=0x100, es=0x1fd, emem_1ff=0x02, emem_100=0x80, emem_1fe=flags('DCZMX'),
             before_regs=before, after_ins=after, after_checks=after_checks)


def brl_tests():
    OPCODE_BRL = 0x82

    # branch forward: 7ef000 = BRL +$7FFF.  7e7002 = jml @ok.  7e7002 = STP
    before = jml_to_ok_instructions(0x7e7002)
    after = '@ok:'
    test('jml $7EF000', ins_name='BRL +$7FFF', before_regs=before, after_ins=after,
         mem_7ef000=OPCODE_BRL, memw_7ef001=0x7fff, mem_7ef003=OPCODE_STP, mem_7f7002=OPCODE_STP, p='')


def clear_set_tests():
    test('clc', p='CZIDXMVN', ep='ZIDXMVN')
    test('clc', p='', ep='')
    test('cld', p='CZIDXMVN', ep='CZIXMVN')
    test('cld', p='', ep='')
    test('cli', p='CZIDXMVN', ep='CZDXMVN')
    test('cli', p='', ep='')
    test('clv', p='CZIDXMVN', ep='CZIDXMN')
    test('clv', p='', ep='')
    test('sec', p='', ep='C')
    test('sec', p=0xFF, ep=0xFF)
    test('sed', p='', ep='D')
    test('sed', p=0xFF, ep=0xFF)
    test('sei', p='', ep='I')
    test('sei', p=0xFF, ep=0xFF)


def cmp_tests():
    modes_a = [A_IMM, A_DIR_X_IND, A_STACK_S, A_DIR, A_IND_LONG_DIR, A_ABS, A_LONG, A_IND_DIR_Y,
               A_IND_DIR, A_IND_STACK_S_Y, A_DIR_X, A_IND_LONG_DIR_Y, A_ABS_Y, A_ABS_X, A_LONG_X]
    modes_xy = [A_IMM, A_DIR, A_ABS]

    def do_test16(val1, val2, p, ep):
        test_ins_with_modes('cmp', modes_a, a=val1, val=val2, p=p, ep=ep)
        test_ins_with_modes('cpx', modes_xy, x=val1, val=val2, p=p, ep=ep)
        test_ins_with_modes('cpy', modes_xy, y=val1, val=val2, p=p, ep=ep)

    def do_test8(val1, val2, p, ep):
        test_ins_with_modes('cmp', modes_a, a=val1, val=val2, p=p + 'M', ep=ep + 'M')
        test_ins_with_modes('cpx', modes_xy, x=val1, val=val2, ex=val1 & 0xFF, p=p + 'X', ep=ep + 'X')
        test_ins_with_modes('cpy', modes_xy, y=val1, val=val2, ey=val1 & 0xFF, p=p + 'X', ep=ep + 'X')

    do_test16(0xABCD, 0xABCD, p='', ep='ZC')
    do_test16(0xABCC, 0xABCD, p='CZIDVN', ep='IDVN')

    do_test8(0xABCD, 0xCD, p='', ep='ZC')
    do_test8(0xABCC, 0xCD, p='CZIDVN', ep='IDVN')


def dec_tests():
    modes = [A_ACCUM, A_DIR, A_ABS, A_DIR_X, A_ABS_X]

    def do_test(val, result, p, ep, bits=16):
        is8 = bits == 8
        test_ins_with_modes('dec', modes, val, result, p=p + is8 * 'M', ep=ep + is8 * 'M')
        test('dex', x=val, ex=result, p=p + is8 * 'X', ep=ep + is8 * 'X')
        test('dey', y=val, ey=result, p=p + is8 * 'X', ep=ep + is8 * 'X')

    do_test(0x0001, 0x0000, p='', ep='Z')
    do_test(0x0000, 0xFFFF, p='CZIDVN', ep='CIDVN')

    do_test(0x01, 0x00, p='', ep='Z', bits=8)
    do_test(0x00, 0xFF, p='CZIDVN', ep='CIDVN', bits=8)


def eor_tests():
    test_ins_with_modes('eor', READ_MODES, a=0xFEFF, val=0x6F8C, p='', ea=0x9173, ep='N')
    test_ins_with_modes('eor', [A_IMM], a=0xAAAA, val=0xAAAA, p='', ea=0x0000, ep='Z')
    test_ins_with_modes('eor', [A_IMM], a=0x1234, val=0x4334, p='CZIDVN', ea=0x5100, ep='CIDV')

    test_ins_with_modes('eor', READ_MODES, a=0x12FE, val=0x6F, p='M', ea=0x1291, ep='MN')
    test_ins_with_modes('eor', [A_IMM], a=0xAAAA, val=0xAA, p='M', ea=0xAA00, ep='MZ')


def inc_tests():
    modes = [A_ACCUM, A_DIR, A_ABS, A_DIR_X, A_ABS_X]

    def do_test(val, result, p, ep, bits=16):
        is8 = bits == 8
        test_ins_with_modes('inc', modes, val, result, p=p + is8 * 'M', ep=ep + is8 * 'M')
        test('inx', x=val, ex=result, p=p + is8 * 'X', ep=ep + is8 * 'X')
        test('iny', y=val, ey=result, p=p + is8 * 'X', ep=ep + is8 * 'X')

    do_test(0xFFFF, 0x0000, p='', ep='Z')
    do_test(0x7FFF, 0x8000, p='CZIDVN', ep='CIDVN')

    do_test(0xFF, 0x00, p='', ep='Z', bits=8)
    do_test(0x7F, 0x80, p='CZIDVN', ep='CIDVN', bits=8)


def jmp_tests():
    # All modes jump to $7e8000, which contains a jml to @ok.
    before = jml_to_ok_instructions(0x7e8000)
    after = f'''
        jsr bank{bank_num}_save_results
        bra @to_fail
    @ok:'''

    # jmp abs (opcode=0x4C): jump from 7e7000.
    test('jml $7e7000', ins_name='jmp $8000', mem_7e7000=0x4c, memw_7e7001=0x8000, mem_7e7003=OPCODE_STP,
         before_regs=before, after_ins=after)

    # jmp long (a.k.a. jml)
    test('jml $7e8000', before_regs=before, after_ins=after, p='CZIDXMVNE')

    # jmp (abs) - opcode=0x6C.  Reads from bank 0. Target stored at test_target (00:FFA2)
    test('jml $7e7000', ins_name='jmp ($FFA2)', dbr=0x7F, mem_7e7000=0x6c, memw_7e7001=0xFFA2, mem_7e7003=OPCODE_STP,
         before_regs=before, after_ins=after)

    # jmp [abs] - opcode=0xDC.  Reads from bank 0. Target stored at test_target24 (00:FFA4)
    test('jml [$FFA4]', dbr=0x7F, before_regs=before, after_ins=after)

    # jmp (abs,x) - opcode=0x7C. Reads from PBR bank. jump from 7e7000, target stored at 7e5000. Wrap in bank.
    test('jml $7e7000', ins_name='jmp ($F000,x)', mem_7e7000=0x7c, memw_7e7001=0xf000, x=0x6000, mem_7e7003=OPCODE_STP,
         memw_7e5000=0x8000, dbr=0x7F, before_regs=before, after_ins=after)
    # with 8 bit index. Target at 7e0080
    test('jml $7e7000', ins_name='jmp ($FFFF,x)', mem_7e7000=0x7c, memw_7e7001=0xffff, x=0x81, mem_7e7003=OPCODE_STP,
         memw_7e0080=0x8000, dbr=0x7F, before_regs=before, after_ins=after, p='X')
    # with E=1
    test('jml $7e7000', ins_name='jmp ($FFFF,x)', mem_7e7000=0x7c, memw_7e7001=0xffff, x=0x81, mem_7e7003=OPCODE_STP,
         memw_7e0080=0x8000, dbr=0x7F, before_regs=before, after_ins=after, p='E', ep='MXE')


def jsr_tests():
    # All modes jump to $7e8000, which contains a jml to @ok.
    before = jml_to_ok_instructions(0x7e8000)
    after = '@ok:'

    # jsr abs (opcode=0x20).
    test('jml $7e7000', ins_name='jsr $8000', mem_7e7000=0x20, memw_7e7001=0x8000, mem_7e7003=OPCODE_STP,
         ememw_01ee=0x7002, es=0x1ed, before_regs=before, after_ins=after)
    # With E=1, the stack pushes wrap within page 1
    test('jml $7e7000', ins_name='jsr $8000', mem_7e7000=0x20, memw_7e7001=0x8000, mem_7e7003=OPCODE_STP, s=0x100, p='CZIDXMVNE',
         emem_01ff=0x02, emem_0100=0x70, es=0x1fe, before_regs=before, after_ins=after)

    # jsl long (opcode=0x22), a.k.a. jsr
    test('jml $7f7000', ins_name='jsl $FE8000', mem_7f7000=0x22, memw_7f7001=0x8000, mem_7f7003=0x7e, mem_7f7004=OPCODE_STP,
         ememw_01ed=0x7003, emem_01ef=0x7f, es=0x1ec, before_regs=before, after_ins=after)
    # With E=1, the stack pushes can write outside page 1
    test('jml $7f7000', ins_name='jsl $FE8000', mem_7f7000=0x22, memw_7f7001=0x8000, mem_7f7003=0x7e, mem_7f7004=OPCODE_STP, s=0x100, p='CZIDXMVNE',
         ememw_00fe=0x7003, emem_0100=0x7f, es=0x1fd, before_regs=before, after_ins=after)

    # jsr (abs,x) (opcode=0xFC).  Reads from PBR bank. Target addr at $7E5000.
    test('jml $7e7000', ins_name='jsr ($F000,x)', mem_7e7000=0xFC, memw_7e7001=0xF000, mem_7e7003=OPCODE_STP, x=0x6000, memw_7e5000=0x8000,
         ememw_01ee=0x7002, es=0x1ed, dbr=0x7F, before_regs=before, after_ins=after)
    # 8-bt index. Target at 7e0080
    test('jml $7e7000', ins_name='jsr ($FFFF,x)', mem_7e7000=0xFC, memw_7e7001=0xFFFF, mem_7e7003=OPCODE_STP, x=0x81, memw_7e0080=0x8000, p='X',
         ememw_01ee=0x7002, es=0x1ed, dbr=0x7F, before_regs=before, after_ins=after)
    # With E=1, the stack pushes can write outside page 1
    test('jml $7e7000', ins_name='jsr ($FFFF,x)', mem_7e7000=0xFC, memw_7e7001=0xFFFF, mem_7e7003=OPCODE_STP, x=0x81, memw_7e0080=0x8000, p='MXE',
         s=0x100, ememw_00ff=0x7002, es=0x1fe, dbr=0x7F, before_regs=before, after_ins=after)


def ld_tests():
    modes_a = [A_IMM, A_DIR_X_IND, A_STACK_S, A_DIR, A_IND_LONG_DIR, A_ABS, A_LONG, A_IND_DIR_Y,
               A_IND_DIR, A_IND_STACK_S_Y, A_DIR_X, A_IND_LONG_DIR_Y, A_ABS_Y, A_ABS_X, A_LONG_X]
    modes_x = [A_IMM, A_DIR, A_ABS, A_DIR_Y, A_ABS_Y]
    modes_y = [A_IMM, A_DIR, A_ABS, A_DIR_X, A_ABS_X]

    def do_test(val, p, ep, bits=16):
        is8 = bits == 8
        test_ins_with_modes('lda', modes_a, val, a=0x1234, ea=val + is8 * 0x1200, p=p + is8 * 'M', ep=ep + is8 * 'M')
        test_ins_with_modes('ldx', modes_x, val, x=0x1234, ex=val, p=p + is8 * 'X', ep=ep + is8 * 'X')
        test_ins_with_modes('ldy', modes_y, val, y=0x1234, ey=val, p=p + is8 * 'X', ep=ep + is8 * 'X')

    do_test(0x8000, p='Z', ep='N')
    do_test(0x0000, p='CZIDVN', ep='CZIDV')
    do_test(0x80, p='Z', ep='N', bits=8)
    do_test(0x00, p='CZIDVN', ep='CZIDV', bits=8)


def lsr_tests():
    modes = [A_ACCUM, A_DIR, A_ABS, A_DIR_X, A_ABS_X]
    test_ins_with_modes('lsr', modes, 0x0001, 0x0000, p='', ep='ZC')
    test_ins_with_modes('lsr', modes, 0x4222, 0x2111, p='CZIDVN', ep='IDV')

    test_ins_with_modes('lsr', modes, 0x01, 0x00, p='M', ep='MZC')
    test_ins_with_modes('lsr', modes, 0x42, 0x21, p='CZIDMVN', ep='IDMV')


def mvn_tests():
    # move between banks. Wrap in each bank.
    test('mvn #$7e, #$7f', x=0xffff, y=0xfffe, a=3, edbr=0x7f, ea=0xffff, ex=0x0003, ey=0x0002,
         mem_7effff=0x21, mem_7e0000=0x22, mem_7e0001=0x23, mem_7e0002=0x24, mem_7f0001=0x00, mem_7f0002=0x99,
         emem_7ffffe=0x21, emem_7fffff=0x22, emem_7f0000=0x23, emem_7f0001=0x24, emem_7f0002=0x99)

    # 16-bit accum even in 8-bit mode. Also test overlapping move backward.
    test('mvn #$7f, #$7f', x=5, y=0, a=0x1ff, edbr=0x7f, ea=0xffff, ex=0x0205, ey=0x0200,
         mem_7f0005=0x31,  mem_7f0204=0x32,  mem_7f0200=0x99,
         emem_7f0000=0x31, emem_7f01ff=0x32, emem_7f0200=0x99, p='CZIDMVN')

    # Overlapping move forward - will create a repeating pattern
    test('mvn #$7f, #$7f', x=0, y=2, a=0x1ff, edbr=0x7f, ea=0xffff, ex=0x200, ey=0x202,
         mem_7f0000=0x41, mem_7f0001=0x42, emem_7f0200=0x41, emem_7f0201=0x42)

    # 8 bit indexes: Wrap in page 0
    test('mvn #$7e, #$7f', x=0x05ff, y=0x05fe, a=3, edbr=0x7f, ea=0xffff, ex=0x0003, ey=0x0002, p='X',
         mem_7e00ff=0x51, mem_7e0000=0x52, mem_7e0001=0x53, mem_7e0002=0x54, mem_7f0001=0x00, mem_7f0002=0x99,
         emem_7f00fe=0x51, emem_7f00ff=0x52, emem_7f0000=0x53, emem_7f0001=0x54, emem_7f0002=0x99)


def mvp_tests():
    # move between banks. Wrap in each bank.
    test('mvp #$7e, #$7f', x=0x0002, y=0x0001, a=3, edbr=0x7f, ea=0xffff, ex=0xfffe, ey=0xfffd,
         mem_7effff=0x21, mem_7e0000=0x22, mem_7e0001=0x23, mem_7e0002=0x24, mem_7f0001=0x00, mem_7ffffd=0x99,
         emem_7ffffe=0x21, emem_7fffff=0x22, emem_7f0000=0x23, emem_7f0001=0x24, emem_7ffffd=0x99)

    # 16-bit accum even in 8-bit mode. Also test overlapping move forward.
    test('mvp #$7f, #$7f', x=0x1ff, y=0x204, a=0x1ff, edbr=0x7f, ea=0xffff, ex=0xffff, ey=0x0004,
         mem_7f0000=0x31,  mem_7f01ff=0x32, mem_7f0004=0x99,
         emem_7f0005=0x31, emem_7f0204=0x32, emem_7f0004=0x99, p='CZIDMVN')

    # Overlapping move backward - will create a repeating pattern
    test('mvp #$7f, #$7f', x=0x201, y=0x1ff, a=0x1ff, edbr=0x7f, ea=0xffff, ex=0x0001, ey=0xffff,
         mem_7f0200=0x41, mem_7f0201=0x42, emem_7f0000=0x41, emem_7f0001=0x42)

    # 8 bit indexes: Wrap in page 0
    test('mvp #$7e, #$7f', x=0x0502, y=0x0501, a=3, edbr=0x7f, ea=0xffff, ex=0x00fe, ey=0x00fd, p='X',
         mem_7e00ff=0x51, mem_7e0000=0x52, mem_7e0001=0x53, mem_7e0002=0x54, mem_7f0001=0x00, mem_7f0002=0x99,
         emem_7f00fe=0x51, emem_7f00ff=0x52, emem_7f0000=0x53, emem_7f0001=0x54, emem_7f0002=0x99)


def nop_wdm_tests():
    for ins in ('nop', 'wdm $AB'):
        test(ins, p='')
        test(ins, p='CZIDXMVNE')


def ora_tests():
    test_ins_with_modes('ora', READ_MODES, a=0x2318, val=0xB939, p='', ea=0xBB39, ep='N')
    test_ins_with_modes('ora', [A_IMM], a=0x0000, val=0x0000, p='', ea=0x0000, ep='Z')
    test_ins_with_modes('ora', [A_IMM], a=0x1200, val=0x4300, p='CZIDVN', ea=0x5300, ep='CIDV')

    test_ins_with_modes('ora', READ_MODES, a=0x1298, val=0x39, p='M', ea=0x12B9, ep='MN')
    test_ins_with_modes('ora', [A_IMM], a=0xAA00, val=0x00, p='M', ea=0xAA00, ep='MZ')


def pea_tests():
    test('pea $ABCD', ememw_01ee=0xabcd, es=0x1ed)
    # E=1: Can push outside page 1
    test('pea $9876', s=0x100, ememw_00ff=0x9876, es=0x1fe, p='CZIDXMVNE')


def pei_tests():
    # set DBR to 7F to make sure the actual read is done from bank 0 and not DBR.
    test('pei ($34)', memw_33=0x8765, d=0xffff, dbr=0x7F, ememw_01ee=0x8765, es=0x1ed)
    # E=1: Can leave stack page 1 and direct page
    test('pei ($ff)', memw_2ff=0x7654, d=0x0200, dbr=0x7F, s=0x100, ememw_00ff=0x7654, es=0x1fe, p='CZIDXMVNE')


def per_tests():
    before = jml_to_ok_instructions(0x7e7003)
    after = '@ok:'
    OPCODE_PER = 0x62

    test('jml $7e7000', ins_name='per -$8000', mem_7e7000=OPCODE_PER, memw_7e7001=0x8000,
         ememw_01ee=0xF003, es=0x1ed, before_regs=before, after_ins=after)
    test('jml $7e7000', ins_name='per +$7FFF', mem_7e7000=OPCODE_PER, memw_7e7001=0x7fff,
         ememw_01ee=0xF002, es=0x1ed, before_regs=before, after_ins=after)
    # E=1: Can push outside page 1
    test('jml $7e7000', ins_name='per -$8000', mem_7e7000=OPCODE_PER, memw_7e7001=0x8000, p='CZIDXMVNE',
         s=0x100, ememw_00ff=0xF003, es=0x1fe, before_regs=before, after_ins=after)


def push_tests():
    test('pha', a=0x9876, es=0x1ed, ememw_1ee=0x9876)
    test('pha', a=0x8765, es=0x1ee, emem_1ef=0x65, p='CZIDVNM')
    test('pha', a=0x5678, s=0x100, es=0x1FF, emem_100=0x78, p='MXE')

    test('phx', x=0x9876, es=0x1ed, ememw_1ee=0x9876)
    test('phx', x=0x23, es=0x1ee, emem_1ef=0x23, p='CZIDVNX')
    test('phx', x=0x34, s=0x100, es=0x1FF, emem_100=0x34, p='MXE')

    test('phy', y=0x1324, es=0x1ed, ememw_1ee=0x1324)
    test('phy', y=0x35, es=0x1ee, emem_1ef=0x35, p='CZIDVNX')
    test('phy', y=0x46, s=0x100, es=0x1FF, emem_100=0x46, p='MXE')

    test('phb', dbr=0x7F, es=0x1ee, emem_1ef=0x7F)
    test('phb', dbr=0x7F, s=0x100, es=0x1ff, emem_100=0x7F, p='CZIDVNMXE')

    test('phd', d=0x6543, es=0x1ed, ememw_1ee=0x6543)
    # PHD with E=1 can leave the stack page
    test('phd', d=0x1357, s=0x100, es=0x1fe, ememw_0ff=0x1357, p='CZIDVNMXE')

    OPCODE_PHK = 0x4B
    test('jml $7e8000', ins_name='phk', mem_7e8000=OPCODE_PHK, es=0x1ee, emem_1ef=0x7E,
         before_regs=jml_to_ok_instructions(0x7e8001), after_ins='@ok:')
    test('jml $7e8000', ins_name='phk', mem_7e8000=OPCODE_PHK, s=0x100, es=0x1ff, emem_100=0x7E, p='CZIDVNMXE',
         before_regs=jml_to_ok_instructions(0x7e8001), after_ins='@ok:')

    test('php', p='DZ', es=0x1ee, emem_1ef=flags('DZ'))
    test('php', p='CZIDVNMXE', s=0x100, es=0x1ff, emem_100=flags('CZIDVNMX'))


def pull_axy_tests():
    def do_test(val, bits=16, p='', ep='', **kwargs):
        is8 = bits == 8
        test('pla', a=0x1234, ea=val + 0x1200 * is8, p=p + 'M' * is8, ep=ep + 'M' * is8, **kwargs)
        test('plx', ex=val, p=p + 'X' * is8, ep=ep + 'X' * is8, **kwargs)
        test('ply', ey=val, p=p + 'X' * is8, ep=ep + 'X' * is8, **kwargs)

    do_test(0xFEDC, memw_1f0=0xFEDC, es=0x1f1, ep='N')
    do_test(0x0000, memw_1f0=0x0000, es=0x1f1, p='', ep='Z')
    do_test(0xEF, memw_1f0=0xEF, es=0x1f0, p='CZIDVN', ep='CIDVN', bits=8)
    do_test(0x00, memw_1f0=0x00, es=0x1f0, p='', ep='Z', bits=8)

    # E=1: wrap in page 1
    do_test(0xCE, mem_100=0xCE, s=0x1ff, es=0x100, p='MXE', ep='MXEN', bits=8)


def pull_misc_tests():
    test('plb', mem_1f0=0xFE, es=0x1f0, edbr=0xFE, ep='N')
    test('plb', mem_1f0=0x00, es=0x1f0, edbr=0x00, p='CIDVNMX', ep='CZIDVMX')
    # E=1: No wrapping in page 1
    test('plb', mem_200=0x3D, s=0x1ff, es=0x100, edbr=0x3D, p='MXE', ep='MXE',
         comment=UNDOCUMENTED_COMMENT)

    test('pld', memw_1f0=0x9753, es=0x1f1, ed=0x9753, ep='N')
    test('pld', memw_1f0=0x0000, es=0x1f1, d=0x9999, ed=0x0000, p='CIDVNMX', ep='CZIDVMX')
    # E=1: No wrapping in page 1
    test('pld', memw_200=0x1356, s=0x1ff, es=0x101, ed=0x1356, p='MXE', ep='MXE')

    test('plp', mem_1f0=0, es=0x1f0, p='CZIDVNMX', ep='')
    test('plp', mem_1f0=flags('MZC'), es=0x1f0, p='', ep='MZC')
    # setting x=1 clears high bytes of X,Y
    test('plp', mem_1f0=flags('X'), es=0x1f0, x=0x2345, y=0x3456, ex=0x45, ey=0x56, p='', ep='X')
    # E=1: Wrap in page 1, force X and M to 1
    test('plp', mem_100=flags('CV'), s=0x1ff, es=0x100, p='MXE', ep='CVMXE')


def rep_set_tests():
    # Setting X=1 should clear high byte of x,y
    test('sep #$FF', a=0x1234, x=0x3456, y=0x5678, ea=0x1234, ex=0x56, ey=0x78, p=0, ep=0xFF)
    test('sep #$0F', p=0x11, ep=0x1F)
    test('rep #$F0', p=0xFF, ep=0x0F)
    test('rep #$0F', p=0xFF, ep=0xF0)
    test('rep #$38', p='MXDZE', ep='MXZE')  # Can't clear M,X when E=1


def rol_tests():
    modes = [A_ACCUM, A_DIR, A_ABS, A_DIR_X, A_ABS_X]
    test_ins_with_modes('rol', modes, 0x8000, 0x0000, p='', ep='ZC')
    test_ins_with_modes('rol', modes, 0x4111, 0x8223, p='CZIDVN', ep='IDVN')

    test_ins_with_modes('rol', modes, 0x80, 0x00, p='M', ep='MZC')
    test_ins_with_modes('rol', modes, 0x41, 0x83, p='CZIDMVN', ep='IDMVN')


def ror_tests():
    modes = [A_ACCUM, A_DIR, A_ABS, A_DIR_X, A_ABS_X]
    test_ins_with_modes('ror', modes, 0x0001, 0x0000, p='', ep='ZC')
    test_ins_with_modes('ror', modes, 0x4222, 0xA111, p='CZIDVN', ep='IDVN')

    test_ins_with_modes('ror', modes, 0x01, 0x00, p='M', ep='MZC')
    test_ins_with_modes('ror', modes, 0x42, 0xA1, p='CZIDMVN', ep='IDMVN')


def rts_rtl_rti_tests():
    before = jml_to_ok_instructions(0x7e0000)
    after = '@ok:'

    # RTS. Return from 7e8000 to 7e0000. The stored PC is ffff.
    OPCODE_RTS = 0x60
    test('jml $7e8000', ins_name='rts', mem_7e8000=OPCODE_RTS, memw_1f0=0xffff, es=0x1f1,
         before_regs=before, after_ins=after)
    # E=1: Wraps in stack page 1
    test('jml $7e8000', ins_name='rts', mem_7e8000=OPCODE_RTS, memw_100=0xffff, s=0x1ff, es=0x101, p='CZIDXMVNE',
         before_regs=before, after_ins=after)

    # RTL. Return to 7e0000. The stored PC is 7e:ffff.
    test('rtl', memw_1f0=0xffff, mem_1f2=0x7e, es=0x1f2,
         before_regs=before, after_ins=after)
    # E=1: No stack page 1 wrapping
    # In case of incorrect wrapping, add a "bad" target
    after_rtl = f'''\
        @not_ok:
            jsr bank{bank_num}_save_results
            bra @to_fail
        @ok:'''
    before_rtl = before + '\n' + jml_to_label_instructions(0x7f1000, '@not_ok')
    test('rtl', memw_200=0xffff, mem_202=0x7e, memw_100=0x0fff, mem_102=0x7f, p='CZIDXMVNE', s=0x1ff, es=0x102,
         before_regs=before_rtl, after_ins=after_rtl)

    # RTI. Return to 7e0000. The stored PC is 7e:0000.
    test('rti', mem_1f0=flags('DN'), memw_1f1=0x0000, mem_1f3=0x7e, es=0x1f3, p='CZ', ep='DN',
         before_regs=before, after_ins=after)
    # E=1: Wraps in stack page 1, doesn't pull pbr
    OPCODE_RTI = 0x40
    test('jml $7e8000', ins_name='rts', mem_7e8000=OPCODE_RTI, mem_100=flags('DN'), memw_101=0x0000, s=0x1ff, es=0x102, p='CZE', ep='DNXME',
         before_regs=before, after_ins=after)


def sbc_binary_tests():
    test_ins_with_modes('sbc', READ_MODES, a=0x9090, val=0x908F, ea=0, p='', ep='CZ')
    test_ins_with_modes('sbc', [A_IMM], a=0x9090, val=0x2000, ea=0x7090, p='C', ep='CV')
    test_ins_with_modes('sbc', [A_IMM], a=0x1234, val=0x1235, ea=0xFFFF, p='CZIV', ep='IN')
    test_ins_with_modes('sbc', [A_IMM], a=0x7000, val=0xA000, ea=0xCFFF, p='', ep='NV')

    test_ins_with_modes('sbc', READ_MODES, a=0xCC90, val=0x8F, ea=0xCC00, p='M', ep='MCZ')
    test_ins_with_modes('sbc', [A_IMM], a=0xCC90, val=0x20, ea=0xCC70, p='MC', ep='MCV')
    test_ins_with_modes('sbc', [A_IMM], a=0xCC12, val=0x13, ea=0xCCFF, p='MCZIV', ep='MIN')
    test_ins_with_modes('sbc', [A_IMM], a=0xCC70, val=0xA0, ea=0xCCCF, p='M', ep='MNV')


def sbc_decimal_tests():
    # The V flag is the same result as for binary subtraction
    test_ins_with_modes('sbc', READ_MODES, a=0x9090, val=0x9089, ea=0, p='D', ep='DCZ')
    test_ins_with_modes('sbc', [A_IMM], a=0x0000, val=0x0001, ea=0x9999, p='DC', ep='DN')
    test_ins_with_modes('sbc', [A_IMM], a=0x1000, val=0x9000, ea=0x2000, p='DC', ep='DV')
    test_ins_with_modes('sbc', [A_IMM], a=0x1000, val=0x9001, ea=0x1999, p='DC', ep='D')
    test_ins_with_modes('sbc', [A_IMM], a=0xAB1D, val=0xF1E2, ea=0x59DB, p='DC', ep='D')

    test_ins_with_modes('sbc', READ_MODES, a=0xCC90, val=0x89, ea=0xCC00, p='DM', ep='DMCZ')
    test_ins_with_modes('sbc', [A_IMM], a=0xCC00, val=0x01, ea=0xCC99, p='DMC', ep='DMN')
    test_ins_with_modes('sbc', [A_IMM], a=0xCC10, val=0x90, ea=0xCC20, p='DMC', ep='DMV')
    test_ins_with_modes('sbc', [A_IMM], a=0xCC10, val=0x91, ea=0xCC19, p='DMC', ep='DM')
    test_ins_with_modes('sbc', [A_IMM], a=0xCCAB, val=0xF1, ea=0xCC5A, p='DMC', ep='DM')


def st_tests():
    modes_a = [A_DIR_X_IND, A_STACK_S, A_DIR, A_IND_LONG_DIR, A_ABS, A_LONG, A_IND_DIR_Y,
               A_IND_DIR, A_IND_STACK_S_Y, A_DIR_X, A_IND_LONG_DIR_Y, A_ABS_Y, A_ABS_X, A_LONG_X]
    modes_x = [A_DIR, A_ABS, A_DIR_Y]
    modes_y = [A_DIR, A_ABS, A_DIR_X]

    def do_test(val, p, bits=16):
        is8 = bits == 8
        orig_val = 0x12 if is8 else 0x1234
        test_ins_with_modes('sta', modes_a, orig_val, result=val, a=val + is8 * 0x5500, p=p + is8 * 'M')
        test_ins_with_modes('stx', modes_x, orig_val, result=val, x=val, ex=val, p=p + is8 * 'X')
        test_ins_with_modes('sty', modes_y, orig_val, result=val, y=val, ey=val, p=p + is8 * 'X')

    do_test(0x8000, p='')
    do_test(0x8000, p='CZIDVN')
    do_test(0x00, p='', bits=8)
    do_test(0x00, p='CZIDVN', bits=8)

    modes_z = [A_DIR, A_DIR_X, A_ABS, A_ABS_X]
    test_ins_with_modes('stz', modes_z, 0x1234, result=0, p='')
    test_ins_with_modes('stz', modes_z, 0x1234, result=0, p='CZIDVN')
    test_ins_with_modes('stz', modes_z, 0x12, result=0, p='M')


def trb_tests():
    modes = [A_DIR, A_ABS]
    test_ins_with_modes('trb', modes, 0x9234, a=0x1630, result=0x8004, p='CZIDVN', ep='CIDVN')
    test_ins_with_modes('trb', modes, 0xAAAA, a=0x5555, result=0xAAAA, p='', ep='Z')

    test_ins_with_modes('trb', modes, 0x92, a=0x16, result=0x80, p='CZIDMVN', ep='CIDMVN')
    test_ins_with_modes('trb', modes, 0xAA, a=0x55, result=0xAA, p='M', ep='MZ')


def tsb_tests():
    modes = [A_DIR, A_ABS]
    test_ins_with_modes('tsb', modes, 0x9234, a=0x1630, result=0x9634, p='CZIDVN', ep='CIDVN')
    test_ins_with_modes('tsb', modes, 0xAAAA, a=0x5555, result=0xFFFF, p='', ep='Z')

    test_ins_with_modes('tsb', modes, 0x92, a=0x16, result=0x96, p='CZIDMVN', ep='CIDMVN')
    test_ins_with_modes('tsb', modes, 0xAA, a=0x55, result=0xFF, p='M', ep='MZ')


def transfer_axy_tests():
    test('tax', a=0x8765, x=0x5678, ex=0x8765, p='', ep='N')
    test('tax', a=0x0000, x=0x5678, ex=0x0000, p='', ep='Z')
    test('tax', a=0x1234, x=0x5678, ex=0x1234, p='CZIDMVN', ep='CIDMV')  # 8 bit A - transfers 16 bits
    test('tax', a=0x87AB, x=0x5678, ex=0xAB, p='X', ep='XN')

    test('tay', a=0x8765, y=0x5678, ey=0x8765, p='', ep='N')
    test('tay', a=0x0000, y=0x5678, ey=0x0000, p='', ep='Z')
    test('tay', a=0x1234, y=0x5678, ey=0x1234, p='CZIDMVN', ep='CIDMV')  # 8 bit A - transfers 16 bits
    test('tay', a=0x87AB, y=0x5678, ey=0xAB, p='X', ep='XN')

    test('txa', x=0x9876, a=0x1234, ea=0x9876, p='', ep='N')
    test('txa', x=0x0000, a=0x1234, ea=0x0000, p='', ep='Z')
    test('txa', x=0x00CD, a=0x1234, ea=0xCD, p='CZIDXVN', ep='CIDXV')  # 8 bit X - transfers 16 bits
    test('txa', x=0xABCD, a=0x1234, ea=0x12CD, p='M', ep='MN')  # 8 bit A - transfers 8 bits
    test('txa', x=0xAB00, a=0x1234, ea=0x1200, p='M', ep='MZ')  # 8 bit A - transfers 8 bits

    test('tya', y=0x9876, a=0x1234, ea=0x9876, p='', ep='N')
    test('tya', y=0x0000, a=0x1234, ea=0x0000, p='', ep='Z')
    test('tya', y=0x00CD, a=0x1234, ea=0xCD, p='CZIDXVN', ep='CIDXV')  # 8 bit Y - transfers 16 bits
    test('tya', y=0xABCD, a=0x1234, ea=0x12CD, p='M', ep='MN')  # 8 bit A - transfers 8 bits
    test('tya', y=0xAB00, a=0x1234, ea=0x1200, p='M', ep='MZ')  # 8 bit A - transfers 8 bits

    test('txy', x=0x8765, y=0x5678, ey=0x8765, p='', ep='N')
    test('txy', x=0x0000, y=0x5678, ey=0x0000, p='CIDMVN', ep='ZCIDMV')
    test('txy', x=0x98, y=0, ey=0x98, p='X', ep='XN')
    test('txy', x=0, y=0x98, ey=0, p='X', ep='XZ')

    test('tyx', y=0x8765, x=0x5678, ex=0x8765, p='', ep='N')
    test('tyx', y=0x0000, x=0x5678, ex=0x0000, p='CIDMVN', ep='ZCIDMV')
    test('tyx', y=0x98, x=0, ex=0x98, p='X', ep='XN')
    test('tyx', y=0, x=0x98, ex=0, p='X', ep='XZ')


def transfer_misc_tests():
    test('tsx', ex=0x1ef, p='CZIDMVN', ep='CIDMV')
    test('tsx', s=0x200, es=0x200, ex=0, p='X', ep='XZ')
    test('tsx', ex=0xef, p='X', ep='XN')

    test('tsc', ea=0x1ef, p='CZIDXVN', ep='CIDXV')
    test('tsc', a=0x1234, s=0x200, es=0x200, ea=0x200, p='M', ep='M')
    # Test TSC flag setting
    after = '''\
        bne @not_ok
        iny
    @not_ok:
        ldx #$1ef
        txs'''
    test('tsc', s=0, y=0, ea=0, ey=1, ex=0x1ef, after_ins=after)
    after = '''\
        bpl @not_ok
        iny
    @not_ok:
        ldx #$1ef
        txs'''
    test('tsc', s=0x8000, y=0, ea=0x8000, ey=1, ex=0x1ef, after_ins=after)

    test('txs', x=0x200, es=0x200, p='CZIDMVN', ep='CZIDMVN')
    test('txs', x=0xff, es=0x1ff, p='MXE', ep='MXE')

    test('tcs', a=0x200, es=0x200, p='CZIDMVN', ep='CZIDMVN')
    test('tcs', a=0x77ff, es=0x1ff, p='MXE', ep='MXE')

    test('tcd', a=0, d=0x1234, ed=0, p='', ep='Z')
    test('tcd', a=0x9876, ed=0x9876, p='CZIDMV', ep='CIDMVN')
    test('tcd', a=0x9876, ed=0x9876, p='MXE', ep='MXEN')

    test('tdc', a=0, d=0x9876, ea=0x9876, p='M', ep='MN')
    test('tdc', a=0x1234, d=0, ea=0, p='CIDXVN', ep='CZIDXV')


def xba_tests():
    test('xba', a=0x9812, ea=0x1298, p='', ep='N')
    test('xba', a=0x00aa, ea=0xaa00, p='', ep='Z')
    test('xba', a=0x9812, ea=0x1298, p='CZIDXMVNE', ep='CIDXMVNE')


def xce_tests():
    test('xce', p='', ep='')
    test('xce', p='ZIDXMVN', ep='ZIDXMVN')
    test('xce', x=0x1234, ex=0x34, y=0x4567, ey=0x67, p='C', ep='MXE')
    test('xce', p='MXE', ep='MXC')
    test('xce', p='C', s=0x3ff, es=0x1ff, ep='MXE')


def additional_tests():
    'Tests not on a specific instruction'

    # Test PC wrapping
    OPCODE_NOP = 0xEA
    # In case of incorrect wrapping, add a "bad" target
    after = f'''\
        @not_ok:
            jsr bank{bank_num}_save_results
            bra @to_fail
        @ok:'''
    before = jml_to_ok_instructions(0x7e0000) + '\n' + jml_to_label_instructions(0x7f0000, '@not_ok')
    test('jml $7EFFFF', ins_name='nop  ; test PC wrapping from $FFFF', before_regs=before, after_ins=after,
         mem_7effff=OPCODE_NOP)


# CZIDXMVN
def add_all_tests():
    additional_tests()
    adc_binary_tests()
    and_tests()
    asl_tests()
    bit_tests()
    branch_tests()
    brk_cop_tests()
    brl_tests()
    clear_set_tests()
    cmp_tests()
    dec_tests()
    eor_tests()
    inc_tests()
    jmp_tests()
    jsr_tests()
    ld_tests()
    lsr_tests()
    mvn_tests()
    mvp_tests()
    ora_tests()
    nop_wdm_tests()
    pea_tests()
    pei_tests()
    per_tests()
    pull_axy_tests()
    pull_misc_tests()
    push_tests()
    rep_set_tests()
    rol_tests()
    ror_tests()
    rts_rtl_rti_tests()
    sbc_binary_tests()
    st_tests()
    trb_tests()
    tsb_tests()
    transfer_axy_tests()
    transfer_misc_tests()
    xba_tests()
    xce_tests()

    # Do decimal tests at end because the behavior is partially undocumented
    adc_decimal_tests()
    sbc_decimal_tests()


def init_bank():
    add_asm(f'''\
        bank{bank_num}_save_results:
            ; At this point we don't know the values of DBR or D, so should use only long addressing
            php
            sep #$20
            .a8
            sta f:result_a
            pla
            sta f:result_p
            pla ; low byte of return addr
            sta f:retaddr
            pla ; high byte of return addr
            sta f:retaddr+1
            clc
            xce  ; E mode = 0, so that stack doesn't wrap (which causes issues for some emulators with jsl)
            php
            pla
            and #$01
            sta f:result_p+1
            lda f:result_a
            jsl save_results
            ldx retaddr
            phx
            ldx result_x
            rts
        ''')


def next_bank():
    global bank_num, tests_in_bank
    bank_num += 1
    tests_in_bank = 0
    add_asm(f'''\
        jml test{test_num:04x}

    .segment "BANK{bank_num}"
    ''')
    init_bank()


def write_table():
    with open('tests_table.inc', 'w') as f:
        for i in range(test_num):
            f.write(f'.faraddr test{i:04x}\n')


def main():
    tests_txt.append('Auto-generated by make_cpu_tests.py\n')

    asm_code.append('; Auto-generated by make_cpu_tests.py\n')
    init_bank()
    add_asm('start_tests:\n')

    add_all_tests()

    with open('tests.txt', 'w') as f:
        f.write('\n'.join(tests_txt) + '\n')

    add_asm('jml success')
    with open('tests.inc', 'w') as f:
        f.write('\n'.join(asm_code) + '\n')

    write_table()


main()
