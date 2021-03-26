%include    "pm.inc"    ; 常量, 宏, 以及一些说明

PageDirBase0        equ 200000h ; 页目录开始地址:  2M
PageTblBase0        equ 201000h ; 页表开始地址:    2M +  4K
PageDirBase1        equ 210000h ; 页目录开始地址:  2M + 64K
PageTblBase1        equ 211000h ; 页表开始地址:    2M + 64K + 4K

LinearAddrDemo  equ 00401000h
ProcFoo         equ 00401000h
ProcBar         equ 00501000h
ProcPagingDemo  equ 00301000h

org 0100h
jmp LABEL_BEGIN

[SECTION .gdt]                                    
LABEL_GDT:          Descriptor         0,                 0, 0                          ; 空描述符
LABEL_DESC_NORMAL:  Descriptor         0,            0ffffh, DA_DRW                     ; Normal 描述符
LABEL_DESC_FLAT_C:  Descriptor         0,           0fffffh, DA_CR | DA_32 | DA_LIMIT_4K; 0 ~ 4G
LABEL_DESC_FLAT_RW: Descriptor         0,           0fffffh, DA_DRW | DA_LIMIT_4K       ; 0 ~ 4G
LABEL_DESC_CODE32:  Descriptor         0,  SegCode32Len - 1, DA_CR | DA_32              ; 非一致代码段, 32
LABEL_DESC_CODE16:  Descriptor         0,            0ffffh, DA_C                       ; 非一致代码段, 16
LABEL_DESC_DATA:    Descriptor         0,       DataLen - 1, DA_DRW                     ; Data
LABEL_DESC_STACK:   Descriptor         0,        TopOfStack, DA_DRWA | DA_32            ; Stack, 32 位
LABEL_DESC_LDT1:    Descriptor         0,         LDTLen1-1, DA_LDT                     ; LDT1
LABEL_DESC_LDT2:    Descriptor         0,         LDTLen2-1, DA_LDT                     ; LDT2
LABEL_DESC_TSS1:    Descriptor         0,         TSSLen1-1, DA_386TSS                  ; TSS1
LABEL_DESC_TSS2:    Descriptor         0,         TSSLen2-1, DA_386TSS                  ; TSS2
LABEL_DESC_VIDEO:   Descriptor   0B8000h,            0ffffh, DA_DRW                     ; 显存首地址

GdtLen  equ $ - LABEL_GDT   ; GDT长度
GdtPtr  dw  GdtLen - 1      ; GDT界限
        dd  0               ; GDT基地址

SelectorNormal      equ LABEL_DESC_NORMAL   - LABEL_GDT
SelectorFlatC       equ LABEL_DESC_FLAT_C   - LABEL_GDT
SelectorFlatRW      equ LABEL_DESC_FLAT_RW  - LABEL_GDT
SelectorCode32      equ LABEL_DESC_CODE32   - LABEL_GDT
SelectorCode16      equ LABEL_DESC_CODE16   - LABEL_GDT
SelectorData        equ LABEL_DESC_DATA     - LABEL_GDT
SelectorStack       equ LABEL_DESC_STACK    - LABEL_GDT
SelectorLDT1        equ LABEL_DESC_LDT1     - LABEL_GDT
SelectorLDT2        equ LABEL_DESC_LDT2     - LABEL_GDT
SelectorTSS1        equ LABEL_DESC_TSS1     - LABEL_GDT
SelectorTSS2        equ LABEL_DESC_TSS2     - LABEL_GDT
SelectorVideo       equ LABEL_DESC_VIDEO    - LABEL_GDT

[SECTION .ldt1]
ALIGN   32
LABEL_LDT1:
LABEL_LDT1_DESC_CODE: Descriptor         0,      CodeALen - 1, DA_C + DA_32       ; Code, 32位
LABEL_DESC_STACK1:    Descriptor         0,       TopOfStack1, DA_DRWA | DA_32    ; Stack1, 32 位

LDTLen1 equ $ - LABEL_LDT1

SelectorLDT1Code equ LABEL_LDT1_DESC_CODE - LABEL_LDT1 + SA_TIL
SelectorStack1   equ LABEL_DESC_STACK1    - LABEL_LDT1 + SA_TIL

[SECTION .ldt2]
ALIGN   32
LABEL_LDT2:
LABEL_LDT2_DESC_CODE: Descriptor         0,      CodeBLen - 1, DA_C + DA_32        ; Code, 32位
LABEL_DESC_STACK2:    Descriptor         0,       TopOfStack2, DA_DRWA | DA_32     ; Stack2, 32 位

LDTLen2 equ $ - LABEL_LDT2

SelectorLDT2Code equ LABEL_LDT2_DESC_CODE - LABEL_LDT2 + SA_TIL
SelectorStack2   equ LABEL_DESC_STACK2    - LABEL_LDT2 + SA_TIL


[SECTION .data1]     ; 数据段
ALIGN   32
[BITS   32]
LABEL_DATA:
; 实模式下使用这些符号
; 字符串
_szPMMessage:        db  "In Protect Mode now. ^-^", 0Ah, 0Ah, 0 ; 进入保护模式后显示此字符串
_szMemChkTitle:      db  "BaseAddrL BaseAddrH LengthLow LengthHigh   Type", 0Ah, 0   ; 进入保护模式后显示此字符串
_szRAMSize           db  "RAM size:", 0
_szReturn            db  0Ah, 0
; 变量
_wSPValueInRealMode  dw  0
_dwMCRNumber:        dd  0                   ; Memory Check Result
_dwDispPos:          dd  (80 * 6 + 0) * 2    ; 屏幕第 6 行, 第 0 列。
_dwMemSize:          dd  0
_ARDStruct:                                 ; Address Range Descriptor Structure
    _dwBaseAddrLow:  dd  0
    _dwBaseAddrHigh: dd  0
    _dwLengthLow:    dd  0
    _dwLengthHigh:   dd  0
_dwType:             dd  0
_PageTableNumber:    dd  0
_SavedIDTR:          dd  0                  ; 用于保存 IDTR
                     dd  0
_SavedIMREG:         db  0                  ; 中断屏蔽寄存器值
_MemChkBuf times 256 db  0
_current             dd  0

szPMMessage     equ _szPMMessage    - $$
szMemChkTitle   equ _szMemChkTitle  - $$
szRAMSize       equ _szRAMSize  - $$
szReturn        equ _szReturn   - $$
dwDispPos       equ _dwDispPos  - $$
dwMemSize       equ _dwMemSize  - $$
dwMCRNumber     equ _dwMCRNumber    - $$
ARDStruct       equ _ARDStruct  - $$
dwBaseAddrLow   equ _dwBaseAddrLow  - $$
dwBaseAddrHigh  equ _dwBaseAddrHigh - $$
dwLengthLow     equ _dwLengthLow    - $$
dwLengthHigh    equ _dwLengthHigh   - $$
dwType          equ _dwType     - $$
MemChkBuf       equ _MemChkBuf  - $$
SavedIDTR       equ _SavedIDTR  - $$
SavedIMREG      equ _SavedIMREG - $$
PageTableNumber equ _PageTableNumber- $$
current         equ _current - $$
DataLen         equ $ - LABEL_DATA

; 全局堆栈段
[SECTION .gs]
ALIGN   32
[BITS   32]
LABEL_STACK:
    times 512 db 0
TopOfStack equ $ - LABEL_STACK - 1

[SECTION .gs1]
ALIGN   32
[BITS   32]
LABEL_STACK1:
    times 512 db 0
TopOfStack1 equ $ - LABEL_STACK1 - 1

[SECTION .gs2]
ALIGN   32
[BITS   32]
LABEL_STACK2:
    times 512 db 0
TopOfStack2 equ $ - LABEL_STACK2 - 1

[SECTION .idt]
ALIGN   32
[BITS   32]
LABEL_IDT:
%rep 32
            Gate    SelectorCode32, SpuriousHandler,      0, DA_386IGate
%endrep
.020h:      Gate    SelectorCode32,    ClockHandler,      0, DA_386IGate

IdtLen  equ $ - LABEL_IDT
IdtPtr  dw  IdtLen - 1  ; 段界限
        dd  0           ; 基地址

[SECTION .s16]
[BITS   16]
LABEL_BEGIN:
    mov ax, cs
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0100h

    ; 得到内存数
    mov ebx, 0
    mov di, _MemChkBuf
.loop:
    mov eax, 0E820h
    mov ecx, 20
    mov edx, 0534D4150h
    int 15h
    jc  LABEL_MEM_CHK_FAIL
    add di, 20
    inc dword [_dwMCRNumber]
    cmp ebx, 0
    jne .loop
    jmp LABEL_MEM_CHK_OK
LABEL_MEM_CHK_FAIL:
    mov dword [_dwMCRNumber], 0
LABEL_MEM_CHK_OK:

    ; 初始化 32 位代码段描述符
    xor eax, eax
    mov ax, cs
    shl eax, 4
    add eax, LABEL_SEG_CODE32
    mov word [LABEL_DESC_CODE32 + 2], ax
    shr eax, 16
    mov byte [LABEL_DESC_CODE32 + 4], al
    mov byte [LABEL_DESC_CODE32 + 7], ah

    ; 初始化数据段描述符
    xor eax, eax
    mov ax, ds
    shl eax, 4
    add eax, LABEL_DATA
    mov word [LABEL_DESC_DATA + 2], ax
    shr eax, 16
    mov byte [LABEL_DESC_DATA + 4], al
    mov byte [LABEL_DESC_DATA + 7], ah

    ; 初始化堆栈段描述符
    xor eax, eax
    mov ax, ds
    shl eax, 4
    add eax, LABEL_STACK
    mov word [LABEL_DESC_STACK + 2], ax
    shr eax, 16
    mov byte [LABEL_DESC_STACK + 4], al
    mov byte [LABEL_DESC_STACK + 7], ah

    ; 初始化 LDT1 在 GDT 中的描述符
    xor eax, eax
    mov ax, ds
    shl eax, 4
    add eax, LABEL_LDT1
    mov word [LABEL_DESC_LDT1 + 2], ax
    shr eax, 16
    mov byte [LABEL_DESC_LDT1 + 4], al
    mov byte [LABEL_DESC_LDT1 + 7], ah

    ; 初始化 LDT2 在 GDT 中的描述符
    xor eax, eax
    mov ax, ds
    shl eax, 4
    add eax, LABEL_LDT2
    mov word [LABEL_DESC_LDT2 + 2], ax
    shr eax, 16
    mov byte [LABEL_DESC_LDT2 + 4], al
    mov byte [LABEL_DESC_LDT2 + 7], ah

    ; 初始化 TSS1 描述符
    xor eax, eax
    mov ax, ds
    shl eax, 4
    add eax, LABEL_TSS1
    mov word [LABEL_DESC_TSS1 + 2], ax
    shr eax, 16
    mov byte [LABEL_DESC_TSS1 + 4], al
    mov byte [LABEL_DESC_TSS1 + 7], ah

    ; 初始化 TSS2 描述符
    xor eax, eax
    mov ax, ds
    shl eax, 4
    add eax, LABEL_TSS2
    mov word [LABEL_DESC_TSS2 + 2], ax
    shr eax, 16
    mov byte [LABEL_DESC_TSS2 + 4], al
    mov byte [LABEL_DESC_TSS2 + 7], ah

    ; 初始化 LDT1 中的描述符
    xor eax, eax
    mov ax, ds
    shl eax, 4
    add eax, LABEL_CODE_A
    mov word [LABEL_LDT1_DESC_CODE + 2], ax
    shr eax, 16
    mov byte [LABEL_LDT1_DESC_CODE + 4], al
    mov byte [LABEL_LDT1_DESC_CODE + 7], ah

    ; 初始化 LDT2 中的描述符
    xor eax, eax
    mov ax, ds
    shl eax, 4
    add eax, LABEL_CODE_B
    mov word [LABEL_LDT2_DESC_CODE + 2], ax
    shr eax, 16
    mov byte [LABEL_LDT2_DESC_CODE + 4], al
    mov byte [LABEL_LDT2_DESC_CODE + 7], ah

    ; 初始化堆栈段描述符
    xor eax, eax
    mov ax, ds
    shl eax, 4
    add eax, LABEL_STACK1
    mov word [LABEL_DESC_STACK1 + 2], ax
    shr eax, 16
    mov byte [LABEL_DESC_STACK1 + 4], al
    mov byte [LABEL_DESC_STACK1 + 7], ah

    ; 初始化堆栈段描述符
    xor eax, eax
    mov ax, ds
    shl eax, 4
    add eax, LABEL_STACK2
    mov word [LABEL_DESC_STACK2 + 2], ax
    shr eax, 16
    mov byte [LABEL_DESC_STACK2 + 4], al
    mov byte [LABEL_DESC_STACK2 + 7], ah

    ; 为加载 GDTR 作准备
    xor eax, eax
    mov ax, ds
    shl eax, 4
    add eax, LABEL_GDT          ; eax <- gdt 基地址
    mov dword [GdtPtr + 2], eax ; [GdtPtr + 2] <- gdt 基地址

    ; 为加载 IDTR 作准备
    xor eax, eax
    mov ax, ds
    shl eax, 4
    add eax, LABEL_IDT         ; eax <- idt 基地址
    mov dword [IdtPtr + 2], eax ; [IdtPtr + 2] <- idt 基地址

    ; 保存 IDTR
    sidt    [_SavedIDTR]

    ; 保存中断屏蔽寄存器(IMREG)值
    in  al, 21h
    mov [_SavedIMREG], al

    ; 加载 GDTR
    lgdt    [GdtPtr]

    ; 关中断
    ;cli

    ; 加载 IDTR
    lidt    [IdtPtr]

    ; 打开地址线A20
    in  al, 92h
    or  al, 00000010b
    out 92h, al

    ; 准备切换到保护模式
    mov eax, cr0
    or  eax, 1
    mov cr0, eax

    ; 真正进入保护模式
    jmp dword SelectorCode32:0; 执行这一句会把 SelectorCode32 装入 cs, 并跳转到 SelectorCode32:0 处

[SECTION .s32]; 32 位代码段. 由实模式跳入.
[BITS   32]
LABEL_SEG_CODE32:
    mov ax, SelectorData
    mov ds, ax          ; 数据段选择子
    mov es, ax
    mov ax, SelectorVideo
    mov gs, ax          ; 视频段选择子
    mov ax, SelectorStack
    mov ss, ax          ; 堆栈段选择子
    mov esp, TopOfStack

    xor eax,eax 
    mov dword [current],eax

    call DispMemSize    ; 显示内存信
    call PagingDemo     ; 演示改变页目录的效果

    mov ax, SelectorTSS1
    ltr ax
    mov ax, SelectorLDT1
    lldt ax  

    call Init8259A
    sti
    jmp SelectorLDT1Code:0

io_delay:
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    ret

; Init8259A ---------------------------------------------------------------------------------------------
Init8259A:
    mov al, 011h
    out 020h, al    ; 主8259, ICW1.
    call    io_delay

    out 0A0h, al    ; 从8259, ICW1.
    call    io_delay

    mov al, 020h    ; IRQ0 对应中断向量 0x20
    out 021h, al    ; 主8259, ICW2.
    call    io_delay

    mov al, 028h    ; IRQ8 对应中断向量 0x28
    out 0A1h, al    ; 从8259, ICW2.
    call    io_delay

    mov al, 004h    ; IR2 对应从8259
    out 021h, al    ; 主8259, ICW3.
    call    io_delay

    mov al, 002h    ; 对应主8259的 IR2
    out 0A1h, al    ; 从8259, ICW3.
    call    io_delay

    mov al, 001h
    out 021h, al    ; 主8259, ICW4.
    call    io_delay

    out 0A1h, al    ; 从8259, ICW4.
    call    io_delay

    ;mov    al, 11111111b   ; 屏蔽主8259所有中断
    mov al, 11111110b       ; 仅仅开启定时器中断
    out 021h, al            ; 主8259, OCW1.
    call    io_delay

    mov al, 11111111b       ; 屏蔽从8259所有中断
    out 0A1h, al            ; 从8259, OCW1.
    call    io_delay

    ret
; Init8259A ---------------------------------------------------------------------------------------------

; int handler ---------------------------------------------------------------
_ClockHandler:
ClockHandler    equ _ClockHandler - $$
    ;jmp $
    mov al, 20h
    out 20h, al             ; 发送 EOI
    mov eax, 1
    mov edx, dword [current]
    cmp ax, dx   
    jz  sec
    mov dword [current], eax
    jmp SelectorTSS2:0
    jmp final
sec:     
    xor eax,eax 
    mov dword [current], eax
    jmp SelectorTSS1:0
final:  
    iretd

_SpuriousHandler:
SpuriousHandler equ _SpuriousHandler - $$
    mov ah, 0Ch                         ; 0000: 黑底    1100: 红字
    mov al, '!'
    mov [gs:((80 * 0 + 75) * 2)], ax    ; 屏幕第 0 行, 第 75 列。
    jmp $
    iretd
; ---------------------------------------------------------------------------

; 测试分页机制 ---------------------------------------------------------------
PagingDemo:
    mov ax, cs
    mov ds, ax
    mov ax, SelectorFlatRW
    mov es, ax

    push LenFoo
    push OffsetFoo
    push ProcFoo
    call MemCpy
    add esp, 12

    push    LenBar
    push    OffsetBar
    push    ProcBar
    call    MemCpy
    add esp, 12

    push    LenPagingDemoAll
    push    OffsetPagingDemoProc
    push    ProcPagingDemo
    call    MemCpy
    add esp, 12

    mov ax, SelectorData
    mov ds, ax          ; 数据段选择子
    mov es, ax

    call SetupPaging    ; 启动分页
    call SelectorFlatC:ProcPagingDemo
    call PSwitch        ; 切换页目录，改变地址映射关系
    ;call SelectorFlatC:ProcPagingDemo
    ret
; ---------------------------------------------------------------------------

; 启动分页机制 ---------------------------------------------------------------
SetupPaging:
    ; 根据内存大小计算应初始化多少PDE以及多少页表
    xor edx, edx
    mov eax, [dwMemSize]
    mov ebx, 400000h            ; 400000h = 4M = 4096 * 1024, 一个页表对应的内存大小
    div ebx
    mov ecx, eax                ; 此时 ecx 为页表的个数，也即 PDE 应该的个数
    test edx, edx
    jz .no_remainder
    inc ecx                     ; 如果余数不为 0 就需增加一个页表
.no_remainder:
    mov [PageTableNumber], ecx  ; 暂存页表个数
                                ; 为简化处理, 所有线性地址对应相等的物理地址. 并且不考虑内存空洞.
                                ; 首先初始化页目录
    mov ax, SelectorFlatRW
    mov es, ax
    mov edi, PageDirBase0       ; 此段首地址为 PageDirBase
    xor eax, eax
    mov eax, PageTblBase0 | PG_P  | PG_USU | PG_RWW
.1:
    stosd
    add eax, 4096               ; 为了简化, 所有页表在内存中是连续的.
    loop    .1
    ; 再初始化所有页表
    mov eax, [PageTableNumber]  ; 页表个数
    mov ebx, 1024               ; 每个页表 1024 个 PTE
    mul ebx
    mov ecx, eax                ; PTE个数 = 页表个数 * 1024
    mov edi, PageTblBase0       ; 此段首地址为 PageTblBase
    xor eax, eax
    mov eax, PG_P  | PG_USU | PG_RWW
.2:
    stosd
    add eax, 4096               ; 每一页指向 4K 的空间
    loop    .2

    mov eax, PageDirBase0
    mov cr3, eax
    mov eax, cr0
    or  eax, 80000000h
    mov cr0, eax
    jmp short .3
.3:
    nop
    ret
; 分页机制启动完毕 -----------------------------------------------------------

; 切换页表 ------------------------------------------------------------------
PSwitch:
    ; 初始化页目录
    mov ax, SelectorFlatRW
    mov es, ax
    mov edi, PageDirBase1   ; 此段首地址为 PageDirBase
    xor eax, eax
    mov eax, PageTblBase1 | PG_P  | PG_USU | PG_RWW
    mov ecx, [PageTableNumber]
.1:
    stosd
    add eax, 4096       ; 为了简化, 所有页表在内存中是连续的.
    loop    .1

    ; 再初始化所有页表
    mov eax, [PageTableNumber]  ; 页表个数
    mov ebx, 1024       ; 每个页表 1024 个 PTE
    mul ebx
    mov ecx, eax        ; PTE个数 = 页表个数 * 1024
    mov edi, PageTblBase1   ; 此段首地址为 PageTblBase
    xor eax, eax
    mov eax, PG_P  | PG_USU | PG_RWW
.2:
    stosd
    add eax, 4096       ; 每一页指向 4K 的空间
    loop    .2

    ; 在此假设内存是大于 8M 的
    mov eax, LinearAddrDemo
    shr eax, 22
    mov ebx, 4096
    mul ebx
    mov ecx, eax
    mov eax, LinearAddrDemo
    shr eax, 12
    and eax, 03FFh  ; 1111111111b (10 bits)
    mov ebx, 4
    mul ebx
    add eax, ecx
    add eax, PageTblBase1
    mov dword [es:eax], ProcBar | PG_P | PG_USU | PG_RWW

    mov eax, PageDirBase1
    mov cr3, eax
    jmp short .3
.3:
    nop
    ret
; ---------------------------------------------------------------------------

PagingDemoProc:
OffsetPagingDemoProc equ PagingDemoProc - $$
    mov eax, LinearAddrDemo
    call eax
    retf
LenPagingDemoAll equ $ - PagingDemoProc

foo:
OffsetFoo equ foo - $$
    mov ah, 0Ch                         ; 0000: 黑底    1100: 红字
    mov al, 'F'
    mov [gs:((80 * 17 + 0) * 2)], ax    ; 屏幕第 17 行, 第 0 列。
    mov al, 'o'
    mov [gs:((80 * 17 + 1) * 2)], ax    ; 屏幕第 17 行, 第 1 列。
    mov [gs:((80 * 17 + 2) * 2)], ax    ; 屏幕第 17 行, 第 2 列。
    ret
LenFoo equ $ - foo

bar:
OffsetBar equ bar - $$
    mov ah, 0Ch                         ; 0000: 黑底    1100: 红字
    mov al, 'B'
    mov [gs:((80 * 18 + 0) * 2)], ax    ; 屏幕第 18 行, 第 0 列。
    mov al, 'a'
    mov [gs:((80 * 18 + 1) * 2)], ax    ; 屏幕第 18 行, 第 1 列。
    mov al, 'r'
    mov [gs:((80 * 18 + 2) * 2)], ax    ; 屏幕第 18 行, 第 2 列。
    ret
LenBar equ $ - bar

; 显示内存信息 ---------------------------------------------------------------
DispMemSize:
    push    esi
    push    edi
    push    ecx

    mov esi, MemChkBuf
    mov ecx, [dwMCRNumber]   ;for(int i=0;i<[MCRNumber];i++){
.loop:                       ;
    mov edx, 5               ;    for(int j=0;j<5;j++){//依次显示:BaseAddrLow, BaseAddrHigh, LengthLow, LengthHigh, Type
    mov edi, ARDStruct       ;    
.1:                          ;
    push dword [esi]         ;
    call DispInt             ;        DispInt(MemChkBuf[j*4]);
    pop eax                  ;
    stosd                    ;        ARDStruct[j*4] = MemChkBuf[j*4];
    add esi, 4               ;
    dec edx                  ;
    cmp edx, 0               ;
    jnz .1                   ;    }
    call    DispReturn       ;    printf("\n");
    cmp dword [dwType], 1    ;    if(Type == AddressRangeMemory){//AddressRangeMemory:1, AddressRangeReserved:2
    jne .2                   ;   
    mov eax, [dwBaseAddrLow] ;
    add eax, [dwLengthLow]   ;
    cmp eax, [dwMemSize]     ;        if(BaseAddrLow + LengthLow > MemSize)
    jb  .2                   ;
    mov [dwMemSize], eax     ;            MemSize = BaseAddrLow + LengthLow;
.2:                          ;    }
    loop    .loop            ;}
                             ;
    call    DispReturn       ;printf("\n");
    push    szRAMSize        ;
    call    DispStr          ;printf("RAM size:");
    add esp, 4               ;                   
    push    dword [dwMemSize];
    call    DispInt          ;DispInt(MemSize);
    add esp, 4               ;
    pop ecx
    pop edi
    pop esi
    ret
; ---------------------------------------------------------------------------

%include "lib.inc"; 库函数
SegCode32Len equ $ - LABEL_SEG_CODE32

[SECTION .codeA]
ALIGN   32
[BITS   32]
LABEL_CODE_A:
    mov ax, SelectorVideo
    mov gs, ax                  ; 视频段选择子(目的)
    ;call SelectorFlatC:ProcPagingDemo
task1:
    mov edi, (80 * 13 + 0) * 2  ; 屏幕第 13 行, 第 0 列。
    mov ah, 0Ch                 ; 0000: 黑底    1100: 红字
    mov al, 'A'
    mov [gs:edi], ax

    sti
    jmp task1

CodeALen equ $ - LABEL_CODE_A

[SECTION .codeB]
ALIGN   32
[BITS   32]
LABEL_CODE_B:
    mov ax, SelectorVideo
    mov gs, ax                  ; 视频段选择子(目的)
    call SelectorFlatC:ProcPagingDemo
task2:
    mov edi, (80 * 13 + 0) * 2  ; 屏幕第 13 行, 第 0 列。
    mov ah, 0Ch                 ; 0000: 黑底    1100: 红字
    mov al, 'B'
    mov [gs:edi], ax

    sti
    jmp task2

CodeBLen equ $ - LABEL_CODE_B

[SECTION .tss1]
ALIGN   32
[BITS   32]
LABEL_TSS1:
        DD  0           ; Back
        DD  0           ; 0 级堆栈
        DD  0           ; 
        DD  0           ; 1 级堆栈
        DD  0           ; 
        DD  0           ; 2 级堆栈
        DD  0           ; 
        DD  200000h     ; CR3
        DD  0           ; EIP
        DD  0           ; EFLAGS
        DD  0           ; EAX
        DD  0           ; ECX
        DD  0           ; EDX
        DD  0           ; EBX
        DD  TopOfStack1 ; ESP
        DD  0           ; EBP
        DD  0           ; ESI
        DD  0           ; EDI
        DD  0           ; ES
        DD  SelectorLDT1Code     ; CS
        DD  SelectorStack1       ; SS
        DD  SelectorData         ; DS
        DD  0           ; FS
        DD  0           ; GS
        DD  SelectorLDT1         ; LDT
        DW  0                    ; 调试陷阱标志
        DW  $ - LABEL_TSS1 + 2   ; I/O位图基址
        DB  0ffh                 ; I/O位图结束标志
TSSLen1 equ $ - LABEL_TSS1

[SECTION .tss2]
ALIGN   32
[BITS   32]
LABEL_TSS2:
        DD  0           ; Back
        DD  0           ; 0 级堆栈
        DD  0           ; 
        DD  0           ; 1 级堆栈
        DD  0           ; 
        DD  0           ; 2 级堆栈
        DD  0           ; 
        DD  210000h     ; CR3
        DD  0           ; EIP
        DD  0           ; EFLAGS
        DD  0           ; EAX
        DD  0           ; ECX
        DD  0           ; EDX
        DD  0           ; EBX
        DD  TopOfStack2 ; ESP
        DD  0           ; EBP
        DD  0           ; ESI
        DD  0           ; EDI
        DD  0           ; ES
        DD  SelectorLDT2Code     ; CS
        DD  SelectorStack2       ; SS
        DD  SelectorData         ; DS
        DD  0           ; FS
        DD  0           ; GS
        DD  SelectorLDT2         ; LDT
        DW  0                    ; 调试陷阱标志
        DW  $ - LABEL_TSS2 + 2   ; I/O位图基址
        DB  0ffh                 ; I/O位图结束标志
TSSLen2 equ $ - LABEL_TSS2