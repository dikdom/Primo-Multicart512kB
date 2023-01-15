; This file can be compiled by the asm80.com online IDE, but I guess you can port onto your
; favourite z80 asm compiler
; 
; Ez a file az asm80.com online IDE-vel készült és ott is fordítottam, de egészen nyugodtan
; portolhatod a kedvenc asm compileredre.



INITRAM     EQU     $3178 
VRAM_ADDR_POI EQU   $0013 
VRAM_ADDR_RAM EQU   $4040 ; same value as in $0013 - VRAM STARt ADDRESS
DSPHND      EQU     $0015 ; print char> input: A
KYBHND      EQU     $001D ; get pressed key> output: A (0x00 if no pressed key)
KKBHND      EQU     $0025 ; get key> output: A (blocks until pressed)
DIRCUR      EQU     $0080 ; posXY> input: DE/sor-oszlop (0..15, 0..41)
MIRROR      EQU     $403B 
STACK_IN_MENU EQU   $6800 
BASIC_START_ADDRESS EQU $43EA 
LOWRAMBLOCK_ADDR EQU $7800 
RAMBLOCK_ADDR EQU   $C000 
RAMBLOCK_VARIABLES_OFSET EQU $0400 
START_RAMJC EQU     $5000 
EXEC_PRI    EQU     $01 
EXEC_PTP    EQU     $02 
EXEC_ROM    EQU     $03 


; .PRI file block type
PRI_BT_BASIC EQU    $D1 
PRI_BT_SCREEN EQU   $D5 
PRI_BT_ASSEMBLY EQU $D9 
; .PRI execution type
PRI_EX_ASSEMBLY EQU $C3 
PRI_EX_BASIC EQU    $C9 
; .PTP block types
PTP_BT_NORMAL EQU   $55     ; note: not used during load 
PTP_BT_CLOSING EQU  $AA     ; note: not used during load
PTP_NBT_BASIC_NAME EQU $83 
PTP_NBT_DATA_NAME EQU $87 
PTP_NBT_BASIC_PROG EQU $F1 
PTP_NBT_SCREEN EQU  $F5 
PTP_NBT_BASIC_DATA EQU $F7 
PTP_NBT_ASSEMBLY_PROG EQU $F9 
PTP_CBT_PROG_END EQU $B1 ; BASIC or Assembly
PTP_CBT_SCREEN_END EQU $B5 
PTP_CBT_BAS_DATA_END EQU $B7 
PTP_CBT_ASSEMBLY_END EQU $B9 


KI4         EQU     $C0 
MENU_ENTRY_POIS EQU $1000 
MAX_LINE    EQU     12 

KEY_UP      EQU     $1F 
KEY_DN      EQU     $0A 
KEY_LT      EQU     $08 
KEY_RT      EQU     $19 
KEY_UP2     EQU     "a" 
KEY_DN2     EQU     "y" 
KEY_LT2     EQU     96 
KEY_RT2     EQU     125 
KEY_SPACE   EQU     $20 
KEY_BRK     EQU     $01 
KEY_RETURN  EQU     $0D 


            .BINFROM $0000 
            .BINTO  END_OF_ROM 

; KI-2 64-127 ($40-$7F)
; KI-4 192-255 ($C0-$FF)
; KI-1
; bit 7 - NMI
;   1 - ENABLED
;   0 - DISABLED
; bit 6 - JOY STEP
;   0-1 change initiates a joy step
; bit 5 - cassette socket pin 4
;   0 - GND
;   1 - +5V
; bit 4 - Piezzo "horn"
; bit 3 - Screen page, start at
;   1 - end-6kByte
;   0 - end-14kByte
; bit 2 - cassette socket pin 5
;   0 - GND
;   1 - +5V
; bit 01 - cassette socket pin 1, DATA OUT
;      00 - -110mV
;  01, 10 - GND
;      11 - +110mV
; MIRROR ADDR: 403B

; a"   e"   o"   u"   o:   u:
; 125  96   91   95   124  126
; 7D   60   5B

            .ORG    $0000 
            DI       
            LD      a,$00 ; no NMI, VIDRAM is the upper 6kB
            OUT     ($00),A 

; menu
            LD      bc,END_JUMPCODE-START_JUMPCODE 
            LD      hl,START_JUMPCODE 
            LD      de,START_RAMJC 
            LDIR     
            LD      SP,STACK_IN_MENU 
            CALL    START_RAMJC 

; check entry type. If ROM then it is executed and no RET
; HL contains the menu entry selected
            PUSH    HL 
            LD      BC,END_ROMPTPPRI_SELECT-START_ROMPTPPRI_SELECT 
            LD      HL,START_ROMPTPPRI_SELECT 
            LD      DE,LOWRAMBLOCK_ADDR 
            LDIR     
            LD      HL,(VRAM_ADDR_RAM) 
            LD      DE,$1000 + RAMBLOCK_VARIABLES_OFSET 
            ADD     HL,DE 
            PUSH    HL 
            POP     IY 
            LD      DE,$800 - RAMBLOCK_VARIABLES_OFSET 
            ADD     HL,DE 
            POP     DE 
            LD      SP,HL 
            EX      DE,HL 
            CALL    LOWRAMBLOCK_ADDR 

            PUSH    HL 
            LD      HL,(VRAM_ADDR_RAM) 
            LD      DE,$1000 
            ADD     HL,DE 
            EX      DE,HL ; DE: 7800, b800, f800

COPY_BLOCKS:         
            PUSH    DE 
            CP      EXEC_PRI 
            JR      z,COPY_PRI_BLOCK 
            LD      BC,END_PTP_COPY - START_PTP_COPY 
            LD      HL,START_PTP_COPY 
            LDIR     
            POP     HL 
            JP      (HL) 
COPY_PRI_BLOCK:      
            LD      BC,END_PRI_COPY - START_PRI_COPY 
            LD      HL,START_PRI_COPY 
            LDIR     
            POP     HL 
            JP      (HL) 
NMI:                 
            .ORG    $0066 
            RETN     


RELATIVE_JUMP_HL:    ; should be using the shadow register set (AF", BC", ...) by now!
            LD      de,(VRAM_ADDR_RAM) 
            ADD     hl,de 
            LD      de,$1000 
            ADD     hl,de 
            JP      (hl) 

START_JUMPCODE:      
            .PHASE  START_RAMJC 
            LD      a,$80 ; enable internal ROM
            OUT     (KI4),A 
            LD      (SELECTED_ROM_BANK),A ; init with $80
            CALL    INITRAM 
            CALL    BASIC_INIT2 

            LD      A,$09 ; disable NMI, Last 6kB of VRAM, magno: 0V
            OUT     ($00),A 

            LD      HL,(VRAM_ADDR_POI) 
            LD      (VRAM_ADDR_RAM),HL 

            CALL    SETUP_SCREEN 

            CALL    COUNT_MENU_ENTRIES 

            XOR     A 
            LD      (DRAW_POS),a 
            LD      (WINDOW_POS),a 
            LD      (SELECTED_POS),a 
            DEC     A 
            LD      (SELECTED_ROM_IDX),a ; init with FF
            LD      a,b 
            LD      (NUM_OF_ENTRIES),a 
            CALL    PRINT_LISTBOX 
            CALL    MENU_LOOP 
            XOR     a 
            OUT     (KI4),A ; enable 0. bank in cart
            RET      

SETUP_SCREEN:        
            LD      DE,$000B ; 0. sor, 11. oszlop
            CALL    DIRCUR 
            LD      HL,MENU_HEADER 
            CALL    PRINT_CSTR 

            LD      DE,$0100 ; 1. sor, 0. oszlop
            CALL    DIRCUR 
            LD      HL,LIST_HEADER 
            CALL    PRINT_CSTR 

            LD      DE,$0E00 
            CALL    DIRCUR 
            LD      HL,MENU_MEMORY 
            CALL    PRINT_CSTR 

            LD      a,(VRAM_ADDR_POI+1) 
            LD      hl,MEMSIZE_32_STR 
            CP      $68 
            JR      z,PRINT_MEMSIZE 
            LD      hl,MEMSIZE_48_STR 
            CP      $A8 
            JR      z,PRINT_MEMSIZE 
            LD      hl,MEMSIZE_64_STR 
PRINT_MEMSIZE:       
            CALL    PRINT_CSTR 

            LD      HL,MEM_KB 
            CALL    PRINT_CSTR 

            LD      DE,$0E1D 
            CALL    DIRCUR 
            LD      HL,MENU_CREDIT 
            CALL    PRINT_CSTR 

            LD      DE,$0F00 
            CALL    DIRCUR 
            LD      HL,USAGE_STR 
            CALL    PRINT_CSTR
            
            LD      A,$FF
            LD      HL,(VRAM_ADDR_POI)
            LD      DE,14*12*32
            ADD     HL,DE
            LD      BC,31
            LD      (HL),A
            PUSH    HL
            POP     DE
            INC     DE
            LDIR

            RET      

MENU_LOOP:           
            CALL    GET_KEY 
            OR      A 
            JR      z,MENU_LOOP 
            LD      HL,KEY_TABLE 
            LD      b,NUM_OF_KEYS 
KEYSEARCH_LOOP:      
            CP      (HL) 
            JR      z,FOUND_ENTRY 
            INC     hl 
            DJNZ    KEYSEARCH_LOOP 
            JR      MENU_LOOP 
FOUND_ENTRY:         
            LD      a,NUM_OF_KEYS 
            SUB     b 
            RLCA     
            LD      e,a 
            LD      d,0 
            LD      hl,JUMP_TABLE 
            ADD     hl,de 
            LD      e,(hl) 
            INC     hl 
            LD      d,(hl) 
            EX      de,hl 
            JP      (hl) 

KEY_TABLE:           
            DB      KEY_UP,KEY_UP2,KEY_DN,KEY_DN2,KEY_LT,KEY_LT2,KEY_RT,KEY_RT2 
            DB      KEY_SPACE,KEY_RETURN,KEY_BRK 
NUM_OF_KEYS EQU     JUMP_TABLE - KEY_TABLE 
JUMP_TABLE:          
            DW      UP_PRESSED,UP_PRESSED,DOWN_PRESSED,DOWN_PRESSED 
            DW      LEFT_PRESSED,LEFT_PRESSED,RIGHT_PRESSED,RIGHT_PRESSED 
            DW      SPACE_PRESSED,RETURN_PRESSED,BRK_PRESSED 

BRK_PRESSED:         
            LD      A,$80 
            OUT     (KI4),A 
            JP      $0000 

UP_PRESSED:          
            LD      A,(SELECTED_POS) 
            OR      A 
            JR      nz,NOT_TOP_SELECTED 
            LD      A,(WINDOW_POS) 
            OR      A 
            JR      z,MENU_LOOP ; we're on top of list
            CALL    MOVE_LIST_DOWN 
            LD      HL,WINDOW_POS 
            DEC     (HL) 
            XOR     A 
            LD      HL,DRAW_POS 
            LD      (HL),A 
            CALL    PRINT_LINE 
            INC     (HL) 
            CALL    PRINT_LINE 
            JP      MENU_LOOP 
NOT_TOP_SELECTED:    
            LD      HL,SELECTED_POS 
            LD      A,(HL) 
            DEC     (HL) 
            LD      HL,DRAW_POS 
            LD      (HL),A 
            CALL    PRINT_LINE 
            DEC     (hl) 
            CALL    PRINT_LINE 
            JP      MENU_LOOP 

DOWN_PRESSED:        
            LD      a,(WINDOW_POS) 
            LD      hl,SELECTED_POS 
            ADD     a,(hl) 
            INC     a 
            LD      hl,NUM_OF_ENTRIES 
            CP      (hl) 
            JP      z,MENU_LOOP 
            LD      hl,SELECTED_POS 
            LD      a,(hl) 
            CP      MAX_LINE-1 
            JR      z,BOTTOM_OF_BOX 
            INC     (hl) 
            LD      hl,DRAW_POS 
            LD      (hl),a 
            CALL    PRINT_LINE 
            INC     (hl) 
            CALL    PRINT_LINE 
            JP      MENU_LOOP 
BOTTOM_OF_BOX:       
            CALL    MOVE_LIST_UP 
            LD      hl,WINDOW_POS 
            INC     (hl) 
            LD      a,MAX_LINE-2 
            LD      hl,DRAW_POS 
            LD      (hl),A 
            CALL    PRINT_LINE 
            INC     (hl) 
            CALL    PRINT_LINE 
            JP      MENU_LOOP 

LEFT_PRESSED:        
            XOR     A 
            LD      (LAST_KEYPRESS),A 
            LD      A,(WINDOW_POS) 
            CP      MAX_LINE 
            JR      nc,PAGE_UP_SIMPLE 
            OR      A 
            LD      A,0 
            JR      nz,NOT_TOP_OF_WINDOW 
            LD      A,(SELECTED_POS) 
            OR      A 
            JP      z,MENU_LOOP 
            PUSH    AF 
            XOR     A 
            LD      (SELECTED_POS),A 
            LD      (DRAW_POS),A 
            CALL    PRINT_LINE 
            POP     AF 
            LD      (DRAW_POS),A 
            CALL    PRINT_LINE 
            JP      MENU_LOOP 
NOT_TOP_OF_WINDOW:   
            LD      (WINDOW_POS),A 
            CALL    PRINT_LISTBOX 
            JP      MENU_LOOP 
PAGE_UP_SIMPLE:      
            LD      hl,WINDOW_POS 
            LD      a,(hl) 
            SUB     MAX_LINE 
            JR      NOT_TOP_OF_WINDOW 

RIGHT_PRESSED:       
            XOR     a 
            LD      (LAST_KEYPRESS),a 
            LD      a,2*MAX_LINE - 1 
            LD      hl,WINDOW_POS 
            ADD     a,(HL) 
            JR      c,ON_LAST_PAGE 
            LD      hl,NUM_OF_ENTRIES 
            CP      (hl) 
            JR      nc,ON_LAST_PAGE 

            LD      A,(WINDOW_POS) 
            ADD     A,MAX_LINE 
PG_DN_WP_IN_A:       
            LD      (WINDOW_POS),A 
PG_DN_DONE:          
            CALL    PRINT_LISTBOX 
            JP      MENU_LOOP 
ON_LAST_PAGE:        
            LD      a,(NUM_OF_ENTRIES) 
            SUB     MAX_LINE 
            JR      nc,PG_DN_WP_OK 
            XOR     a 
PG_DN_WP_OK:         
            LD      hl,WINDOW_POS 
            CP      (hl) 
            JR      z,PG_DN_SET_SELECTION 
            JR      PG_DN_WP_IN_A 
PG_DN_SET_SELECTION:  
            OR      a 
            JR      nz,PG_DN_SELECT_LAST_LINE 
            LD      a,(NUM_OF_ENTRIES) 
            DEC     a 
            JR      PG_DN_LAST_LINE_SELECTED 
PG_DN_SELECT_LAST_LINE:  
            LD      A,MAX_LINE-1 
PG_DN_LAST_LINE_SELECTED:  
            LD      hl,SELECTED_POS 
            CP      (hl) 
            JP      z,MENU_LOOP 
            PUSH    AF 
            LD      A,(SELECTED_POS) 
            LD      (DRAW_POS),A 
            POP     AF 
            LD      (SELECTED_POS),A 
            CALL    PRINT_LINE 
            LD      A,(SELECTED_POS) 
            LD      (DRAW_POS),A 
            CALL    PRINT_LINE 
            JP      MENU_LOOP 

SPACE_PRESSED:       
; Check if current line is ROM
            LD      a,(SELECTED_POS) 
            LD      (DRAW_POS),a 
            CALL    COPY_DIRECTORY_ENTRY 
            LD      a,(TF_TYPE) 
            CP      $03 
            JP      nz,MENU_LOOP ; if not ROM leave now

; check if some ROM is already selected
            LD      a,(SELECTED_ROM_IDX) 
            INC     a 
            JR      z,SET_ROM_SELECT ; if no ROM selected, let's jump to select the current
; check if this ROM is selected earlier?
            LD      a,(WINDOW_POS) 
            LD      hl,SELECTED_POS 
            ADD     a,(hl) 
            LD      hl,SELECTED_ROM_IDX 
            CP      (hl) 
            JR      z,UNSELECT_ROM ; this ROM was selected, let's unselect it!
; other rom was selected, check if reprint needed (no unselection needed...)
            LD      a,(hl) 
            LD      hl,WINDOW_POS 
            SUB     (hl) 
            JR      c,SET_ROM_SELECT ; 
            CP      MAX_LINE 
            JR      nc,SET_ROM_SELECT 
; let's redraw unselected visible line
            LD      (DRAW_POS),a 
            LD      a,$FF 
            LD      (SELECTED_ROM_IDX),a 
            CALL    PRINT_LINE 
            LD      a,(SELECTED_POS) 
            LD      (DRAW_POS),a 
            JR      SET_ROM_SELECT 
UNSELECT_ROM:        
            LD      a,$80 
            LD      (SELECTED_ROM_BANK),a 
            LD      a,$FF 
FINISH_ROM_SELECT:   
            LD      (SELECTED_ROM_IDX),a 
            CALL    PRINT_LINE 
            JP      MENU_LOOP 
SET_ROM_SELECT:      
            LD      a,(TF_BANK) 
            LD      (SELECTED_ROM_BANK),A 
            LD      a,(WINDOW_POS) 
            LD      hl,SELECTED_POS 
            ADD     a,(hl) 
            JR      FINISH_ROM_SELECT 

RETURN_PRESSED:      
            LD      a,(SELECTED_ROM_BANK) 
            LD      c,a 
            LD      hl,WINDOW_POS 
            LD      a,(SELECTED_POS) 
            ADD     a,(hl) 
            LD      l,a 
            LD      h,c 

            PUSH    hl 

WAIT_4_KEYRELEASE_AND_GO:  
            CALL    KYBHND 
            OR      a 
            JR      nz,WAIT_4_KEYRELEASE_AND_GO 
            XOR     a 
            OUT     (KI4),a 
            POP     hl 
            RET      

MOVE_LIST_DOWN:      
            LD      hl,(VRAM_ADDR_POI) 
            LD      de,(2+MAX_LINE-1)*12*32
            ADD     hl,de 
            PUSH    hl 
            LD      de,12*32 
            ADD     hl,de 
            EX      de,hl 
            POP     hl 
            LD      bc,(MAX_LINE-2) * 12*32 
            LDDR     
            RET      
MOVE_LIST_UP:        
            LD      hl,(VRAM_ADDR_POI) 
            LD      de,2*12*32 
            ADD     hl,de 
            PUSH    hl 
            LD      de,12*32 
            ADD     hl,de 
            POP     de 
            LD      bc,(MAX_LINE-2) * 12*32 
            LDIR     
            RET      
PRINT_LISTBOX:       
            XOR     a 
            LD      (DRAW_POS),a 
DRAW_LINE_LOOP:      
            CALL    PRINT_LINE 
            LD      hl,DRAW_POS 
            INC     (HL) 
            LD      a,(WINDOW_POS) 
            LD      hl,DRAW_POS 
            ADD     a,(hl) 
            LD      hl,NUM_OF_ENTRIES 
            CP      (hl) 
            RET     z 
            LD      hl,DRAW_POS 
            LD      a,MAX_LINE 
            CP      (hl) 
            JR      nz,DRAW_LINE_LOOP 
            RET      

PRINT_LINE:          
            PUSH    hl 
            LD      hl,WINDOW_POS 
            LD      a,(DRAW_POS) 
            ADD     a,(hl) 
            LD      (FILEIDX_DRAWN),a 
            CALL    COPY_DIRECTORY_ENTRY 
            CALL    CLEAR_LINE 
            LD      a,(DRAW_POS) 
            ADD     a,2 
            LD      d,a 
            LD      e,0 
            CALL    DIRCUR 
            LD      a,2 
            CALL    PRINT_SPACES 
            LD      hl,SELECTED_ROM_IDX 
            LD      a,(FILEIDX_DRAWN) 
            CP      (hl) 
            JR      nz,NO_BOOT_ROM 
            LD      a,"*" 
            DB      $2a ; LD hl,(nnnn)
NO_BOOT_ROM:         
            LD      a," " 
            CALL    DSPHND 
            LD      a,4 
            CALL    PRINT_SPACES 

            LD      a,(TF_TYPE) 
            CP      $01 
            JR      nz,TYPE_NO_PRI 
            LD      hl,TYPE_PRI_STR 
            JR      TYPE_IN_HL 
TYPE_NO_PRI:         
            CP      $02 
            JR      nz,TYPE_NO_PTP 
            LD      hl,TYPE_PTP_STR 
            JR      TYPE_IN_HL 
TYPE_NO_PTP:         
            LD      hl,TYPE_ROM_STR 
TYPE_IN_HL:          
            CALL    PRINT_CSTR 
            LD      a,3 
            CALL    print_spaces 
            LD      hl,(TF_SIZE) 
            CALL    PRINT_16BIT_DECIMAL_NUMBER 

            LD      a,2 
            CALL    print_spaces 

            LD      hl,TF_NAME 
            CALL    PRINT_CSTR 

            LD      a,(DRAW_POS) 
            LD      hl,SELECTED_POS 
            CP      (hl) 
            JR      nz,SKIP_INVERT 
            LD      b,a 
            INC     b 
            LD      hl,(VRAM_ADDR_POI) 
            LD      de,32*12 
            ADD     hl,de 
            LD      de,32*12 
MUL_LOOP:            
            ADD     hl,de 
            DJNZ    MUL_LOOP 
            LD      bc,32*12 
NEG_LOOP:            
            LD      a,(hl) 
            CPL      
            LD      (hl),a 
            INC     hl 
            DEC     bc ; 6
            LD      a,b ; 4
            OR      c ; 4
            JR      nz,NEG_LOOP 

SKIP_INVERT:         
            POP     hl 
            RET      

PRINT_SPACES:        
            LD      b,a 
PS_LOOP:             
            PUSH    bc 
            LD      a," " 
            CALL    DSPHND 
            POP     bc 
            DJNZ    PS_LOOP 
            RET      
CLEAR_LINE:          
            LD      a,(DRAW_POS) 
            LD      hl,(VRAM_ADDR_POI) 
            LD      de,2*12*32 
            ADD     hl,de 
            LD      de,12*32 
CL_LOOP:             
            OR      a 
            JR      z,CLEAR_IT 
            ADD     hl,de 
            DEC     a 
            JR      CL_LOOP 
CLEAR_IT:            
            LD      bc,32*12-1 
            LD      (HL),a 
            EX      de,hl 
            LD      hl,1 
            ADD     hl,de 
            EX      de,hl 
            LDIR     
            RET      

COPY_DIRECTORY_ENTRY:  
            XOR     a 
            OUT     (KI4),a 
            LD      h,a 
            LD      a,(WINDOW_POS) 
            LD      l,a 
            LD      a,(DRAW_POS) 
            ADD     a,l 
            LD      l,a 
            RL      l 
            RL      h 
            LD      de,$1000 
            ADD     hl,de 
            LD      e,(hl) 
            INC     hl 
            LD      d,(hl) 
            EX      de,hl 
            LD      a,h 
            RLCA     
            RLCA     
            AND     $03 
            OUT     (KI4),A 
            LD      A,H 
            AND     $3F 
            LD      H,A 
            LD      DE,TEMP_FILE_ENTRY 
            LD      BC,6 
            LDIR     
            LD      B,22 
COPY_DE_LOOP:        
            LD      a,(hl) 
            LD      (de),a 
            OR      a 
            JR      z,copy_de_strend 
            INC     hl 
            INC     de 
            DJNZ    copy_de_loop 
            JR      copy_de_done 
COPY_DE_STREND:      
            LD      a,$20 
COPY_DE_STREND_LOOP:  
            LD      (de),a 
            INC     de 
            DJNZ    copy_de_strend_loop 
COPY_DE_DONE:        
            XOR     a 
            LD      (de),a 
            LD      a,$80 
            OUT     (KI4),a 
            RET      

GET_DECIMAL_DIGIT:   
            INC     C 
            SBC     HL,DE 
            JR      nc,GET_DECIMAL_DIGIT 
            DEC     C 
            ADD     HL,DE 
            RET      

PRINT_16BIT_DECIMAL_NUMBER:  
            PUSH    AF 
            PUSH    BC 
            PUSH    DE 
            PUSH    HL 
            LD      B,0 
            LD      c,0 
PRINT_DECIMAL_LOOP:  
            PUSH    bc 
            LD      a,c 
            RLC     a ; MUL by 2
            LD      de,DECIMAL_NUMS 
            ADD     a,e 
            LD      e,a 
            LD      a,$00 
            ADC     a,d 
            LD      d,a 
            PUSH    hl 
            EX      de,hl 
            LD      e,(hl) 
            INC     hl 
            LD      d,(hl) 
            POP     hl 

            LD      c,0 
            CALL    GET_DECIMAL_DIGIT 
            LD      a,e 
            CP      1 
            JR      z,PRINT_DIGIT ; last digit, must print even on zero
            LD      A,B 
            OR      C 
            JR      z,SKIP_DIGIT_PRINT 
PRINT_DIGIT:         
            INC     B 
            LD      A,C 
            ADD     a,"0" 
            PUSH    bc 
            CALL    DSPHND 
            POP     bc 
            JR      SKIP_SPACE_PRINT 
SKIP_DIGIT_PRINT:    
            LD      a," " 
            PUSH    bc 
            CALL    DSPHND 
            POP     bc 
SKIP_SPACE_PRINT:    
            LD      a,b 
            POP     bc 
            INC     c 
            LD      b,a 
            LD      a,c 
            CP      5 
            JR      nz,PRINT_DECIMAL_LOOP 

            POP     HL 
            POP     DE 
            POP     BC 
            POP     AF 
            RET      

DECIMAL_NUMS:        
            DW      10000 
            DW      1000 
            DW      100 
            DW      10 
            DW      1 

PRINT_CSTR:          
            PUSH    hl 
            PUSH    af 
PRINT_LOOP:          
            LD      a,(hl) 
            OR      a 
            JR      z,DONE_PRINT_STR 
            CALL    DSPHND 
            INC     HL 
            JR      PRINT_LOOP 
DONE_PRINT_STR:      
            POP     af 
            POP     hl 
            RET      

BASIC_INIT2:         
            LD      de,$4080 
            LD      hl,$18f7 
            LD      bc,$002a 
            LDIR     
            LD      hl,$41e5 
            LD      (hl),3ah 
            INC     hl 
            LD      (hl),b 
            INC     hl 
            LD      (hl),2ch 
            INC     hl 
            LD      (40a7h),hl 
            LD      de,1997h 
            LD      b,1ch 
            LD      hl,4152h 
BI2_LOOP1:           
            LD      (hl),0c3h 
            INC     hl 
            LD      (hl),e 
            INC     hl 
            LD      (hl),d 
            INC     hl 
            DJNZ    BI2_LOOP1 
            LD      b,15h 
BI2_LOOP2:           
            LD      (hl),0c9h 
            INC     hl 
            INC     hl 
            INC     hl 
            DJNZ    BI2_LOOP2 
            LD      hl,42e8h 
            LD      (hl),b 
            LD      hl,43e9h 
            LD      (hl),b 
            CALL    1b8fh 
; CLS
;            ld a,0ch
;            call 0015h
            LD      hl,(0013h) 
            DEC     hl 
            LD      de,0ffceh 
            LD      (40b1h),hl 
            ADD     hl,de 
            LD      (40a0h),hl 
            CALL    1b4dh 
; PRINT PRIMO Boot text
;            ld hl,0167h
;            call 28a7h

;           Let's revert the SP
            LD      SP,STACK_IN_MENU - 4 
            RET      

COUNT_MENU_ENTRIES:  
            XOR     a 
            OUT     (KI4),a 
            LD      b,a 
            LD      hl,MENU_ENTRY_POIS 
CM_LOOP:             
            LD      a,(hl) 
            LD      c,a 
            INC     hl 
            LD      a,(hl) 
            OR      c 
            JR      z,CM_FINISHED 
            INC     b 
            INC     hl 
            LD      a,h 
            CP      $12 
            JR      nz,CM_LOOP 
CM_FINISHED:         
            XOR     a 
            OUT     (KI4),a 
            RET      

GET_KEY:             
            CALL    KYBHND 
            LD      hl,LAST_KEYPRESS 
            CP      (hl) 
            JR      nz,NEW_KEY_PRESSED 
            OR      a 
            RET     z 
            LD      hl,KEY_REPEAT_CNT 
            DEC     (hl) 
            JR      nz,GET_KEY 
            LD      c,a 
            LD      a,SHORT_REPEAT 
            LD      (hl),a 
            LD      a,c 
            RET      
NEW_KEY_PRESSED:     
            LD      (HL),A 
            LD      A,LONG_REPEAT 
            LD      (KEY_REPEAT_CNT),A 
            LD      A,(LAST_KEYPRESS) 
            RET      

SHORT_REPEAT EQU    $05 
LONG_REPEAT EQU     $0A 

LAST_KEYPRESS:       
            DB      $FF 
KEY_REPEAT_CNT:      
            DB      $00 
MENU_HEADER:         
            .CSTR   "Primo - 512kB Cartridge" 
LIST_HEADER:         
            DB      $05 
            DB      "boot  tipus  m\u0060ret  n\u0060v                   " 
            DB      $15,$00 
USAGE_STR:           
            .CSTR   "Haszn\u007Dlhat\u005B: Nyilak (AY\u0040\u005D), BRK, SPC, RET" 
MENU_CREDIT:         
            .CSTR   "Sanyi - v1.10" 
MENU_MEMORY:         
            .CSTR   "Mem\u005Bria: " 

TYPE_PRI_STR:        
            .CSTR   "pri" 
TYPE_PTP_STR:        
            .CSTR   "ptp" 
TYPE_ROM_STR:        
            .CSTR   "rom" 
MEMSIZE_32_STR:      
            .CSTR   "32" 
MEMSIZE_48_STR:      
            .CSTR   "48" 
MEMSIZE_64_STR:      
            .CSTR   "64" 
MEM_KB:              
            .CSTR   "kB" 

            .DEPHASE  
END_JUMPCODE:        


START_ROMPTPPRI_SELECT:  
            .PHASE  LOWRAMBLOCK_ADDR 
            .BLOCK  ROMPTPPRI 
; INPUT is in HL
; L contains the index of the ROM or the PRG to be started
; H contains the index of the ROM to be switched on during
;   program execution. If none selected it is $80
;            LD      (START_PRG_ROM_IDX),HL ; L->START_PRG_ROM_IDX, H->START_ROM_BANK
            LD      (IY + START_PRG_ROM_IDX_OFS),L 
            LD      (IY + START_ROM_BANK_OFS),H 
            BIT     7,H 
            JR      nz,SKIP_REINIT ; no ROM selected, init is already executed
            LD      A,H ; the selected ROM should be used
            OUT     (KI4),A 
            LD      A,$01 
            CALL    DSPHND ; HL and IY persist

SKIP_REINIT:         
            XOR     A 
            OUT     (KI4),A 
            LD      H,A 
            RL      L 
            RL      H 
            LD      DE,MENU_ENTRY_POIS 
            ADD     HL,DE 
            LD      E,(HL) 
            INC     HL 
            LD      D,(HL) 
            EX      DE,HL 
            LD      A,H 
            RLCA     
            RLCA     
            AND     $03 
            OUT     (KI4),A 
            LD      A,H 
            AND     $3F 
            LD      H,A 
            LD      A,(HL) 
            CP      EXEC_ROM ; is it a ROM?
            LD      DE,$0003 
            ADD     HL,DE ; hl points to bank-16k_addr
            LD      C,KI4 
            LD      B,0 
            OUT     (C),B 
            RET     nz ; if it is not ROM: return
            LD      A,(HL) ; bank in A
            OUT     (KI4),a 
            JP      $0000 ; reset the soft-rom
            .ENDBLOCK  
            .DEPHASE  
END_ROMPTPPRI_SELECT:  


            .MACRO  block_pointer_ops 
; increments a bank-4k address. Bank in B (0-$3F), 4k addr in HL (0-$3FFF)
INC_B16K_ADDR:       
            RS_HDR   
            PUSH    AF 
            INC     HL 
INC_B16K_ADDR_CHECK:  
            LD      a,h 
            CP      $40 
            JR      nz,INC_B16K_ADDR_DONE 
            LD      h,$00 
            INC     b 
INC_B16K_ADDR_DONE:  
            POP     AF 
            RET      

INC2_B16K_ADDR:      
            RS_HDR   
            PUSH    AF 
            INC     HL 
            INC     HL 
            JR      INC_B16K_ADDR_CHECK 

;DEC2_B16K_ADDR:      
;            PUSH    AF 
;            DEC     HL 
;            DEC     HL 
;            LD      a,h 
;            CP      $FF 
;            JR      nz,DEC2_B16K_ADDR_DONE 
;            DEC     b 
;            LD      h,$3f 
;DEC2_B16K_ADDR_DONE:  
;            POP     AF 
;            RET      

; Returns a word from ext-ROM, pointed by HL, retrieved word in DE
; The ext-ROM will be mapped in after this call
GET_WORD_FROM_ROM:   
            RS_HDR   
            PUSH    AF 
            PUSH    HL 
            PUSH    BC 
;            CALL    GET_BYTE_FROM_ROM
            RS_CALL GET_BYTE_FROM_ROM 
            LD      E,A 
;            CALL    inc_b16k_addr
            RS_CALL inc_b16k_addr 
;            CALL    GET_BYTE_FROM_ROM
            RS_CALL GET_BYTE_FROM_ROM 
            LD      D,A 
            POP     BC 
            POP     HL 
            POP     AF 
            RET      

; Returns a byte from ext-ROM, pointed by HL, retrieved byte in A
; The ext-ROM will be mapped in after this call
GET_BYTE_FROM_ROM:   
            RS_HDR   
            LD      A,B 
            OUT     (KI4),A 
            LD      A,(HL) 
            RET      


PRINT_2ND_CSTR:      
            RS_HDR   
            PUSH    hl 
            PUSH    af 
            XOR     A 
            OUT     (KI4),A 
PRINT_2NDLOOP:       
            LD      A,(hl) 
            OR      A 
            JR      z,DONE_2NDPRINT_STR 
            CALL    DSPHND 
            INC     HL 
            JR      PRINT_2NDLOOP 
DONE_2NDPRINT_STR:   
            POP     AF 
            POP     HL 
            RET      
            .ENDM    

            .MACRO  print_hex 
PRINT_HEX_NIBBLE:    
            RS_HDR   
            LD      bc,$8000 + KI4 
            OUT     (C),B 
            CP      10 
            JR      nc,PRINT_HEX_ALPHA 
            ADD     a,"0" 
            CALL    DSPHND 
            RET      
PRINT_HEX_ALPHA:     
            ADD     a,"A" 
            SUB     10 
            CALL    DSPHND 
            RET      

PRINT_HEX_8:         
            RS_HDR   
            PUSH    af
            RLCA     
            RLCA     
            RLCA     
            RLCA     
            AND     $0F 
;            CALL    PRINT_HEX_NIBBLE
            RS_CALL PRINT_HEX_NIBBLE 
            POP     af 
            AND     $0F 
;            CALL    PRINT_HEX_NIBBLE
            RS_CALL PRINT_HEX_NIBBLE 
            RET      

PRINT_HEX_16:        
            RS_HDR   
            PUSH    hl 
            LD      a,h 
;            CALL    PRINT_HEX_8
            RS_CALL PRINT_HEX_8 
            POP     hl 
            LD      a,l 
;            CALL    PRINT_HEX_8
            RS_CALL PRINT_HEX_8 
            RET      
            .ENDM    

            .MACRO  debug_ptp 
WAIT_FOR_SPC:        
            IN      a,($19) 
            AND     $01 
            JR      z,wait_for_spc 

WFL:                 
            LD      b,$80 
WFM:                 
            IN      a,($19) 
            AND     $01 
            JR      nz,wfl 
            DJNZ    wfm 
            RET      

PRINT_SPC:           
            LD      a," " 
            CALL    DSPHND 
            RET      

DEBUG:               
            PUSH    bc 
            PUSH    de 
            PUSH    hl 
            PUSH    af 

            PUSH    hl 
            PUSH    de 

            LD      a,$80 
            OUT     (KI4),a 
            LD      DE,$0000 ; 0. sor, 11. oszlop
            CALL    DIRCUR 
            LD      a,6 
            CALL    DSPHND 

            LD      h,b 
            LD      l,c 
            CALL    PRINT_HEX_16 
            CALL    print_spc 

            POP     de 
            LD      h,d 
            LD      l,e 
            CALL    PRINT_HEX_16 
            CALL    PRINT_spc 

            POP     hl 
            CALL    PRINT_HEX_16 
            CALL    PRINT_spc 

            LD      a,"-" 
            CALL    DSPHND 
            CALL    print_spc 
            LD      a,(SRC_BANK) 
            CALL    PRINT_HEX_8 
            CALL    print_spc 
            CALL    wait_for_spc 

            POP     af 
            POP     hl 
            POP     de 
            POP     bc 
            RET      
            .ENDM    

            .MACRO  rs_jp 
            EXX      ; 4T, 1b
            EX      AF,AF' ; 4T, 1b
            XOR     A ; 4T, 1b
            OUT     (KI4),A ; 11T, 2b
            LD      HL,%%1 - RAMBLOCK_ADDR 
            JP      RELATIVE_JUMP_HL 
            .ENDM    

            .MACRO  rs_jpz 
            JR      nz,NO_JUMP_%%1 
            EXX      ; 4T, 1b
            EX      AF,AF' ; 4T, 1b
            XOR     A ; 4T, 1b
            OUT     (KI4),A ; 11T, 2b
            LD      HL,%%1 - RAMBLOCK_ADDR 
            JP      RELATIVE_JUMP_HL 
            NO_JUMP_%%1:             
            .ENDM    

            .MACRO  rs_call 
            EXX      
            EX      AF,AF' 
            LD      A,$00 
            OUT     (KI4),A 
            LD      HL,%%1 - RAMBLOCK_ADDR 
            CALL    RELATIVE_JUMP_HL 
            .ENDM    

            .MACRO  rs_hdr 
            EXX      ; 4T
            EX      af,af' ; 4T
            .ENDM    


START_PTP_COPY:      
            .PHASE  RAMBLOCK_ADDR 
            .BLOCK  PTP 
            POP     HL 
            LD      A,(HL) 
;            LD      (NEXT_BLOCK_START_BANK),A
            LD      (IY + NEXT_BLOCK_START_BANK_OFS),A 
            LD      B,A ; B is the bank addr
            INC     HL 
            LD      E,(HL) 
            INC     HL 
            LD      D,(HL) 
            EX      DE,HL ; HL is the 16k addr within the bank
;            CALL    INC_B16K_ADDR ; let's skip the FFh and primo file length
            RS_CALL INC_B16K_ADDR 
;            CALL    INC2_B16K_ADDR
            RS_CALL INC2_B16K_ADDR 
            RS_HDR   
PROCESS_PTP_BLOCK:   
            RS_HDR   
;            CALL    INC_B16K_ADDR ; let's skip the block type (NORMAL or CLOSING)
            RS_CALL INC_B16K_ADDR 
;            CALL    GET_WORD_FROM_ROM ; block len
            RS_CALL GET_WORD_FROM_ROM 
;            CALL    INC2_B16K_ADDR
            RS_CALL INC2_B16K_ADDR 
;            CALL    GET_BYTE_FROM_ROM ; block sub-type
            RS_CALL GET_BYTE_FROM_ROM 
;            LD      (BLOCK_TYPE),A
            LD      (IY + BLOCK_TYPE_OFS),A 
            CP      PTP_NBT_BASIC_NAME 
            JR      z,PTP_NAME_BLOCK 
            CP      PTP_NBT_DATA_NAME 
            JR      z,PTP_NAME_BLOCK 
            JR      PTP_DATA_OR_END_BLOCK 
PTP_NAME_BLOCK:      
            ADD     hl,de 
            LD      de,$4000 
            XOR     a 
            SBC     hl,de 
            JR      nc,BLOCK_OVERFLOW 
            ADD     hl,de 
            JR      STORE_NEXT_BLOCK_START 
BLOCK_OVERFLOW:      
            INC     b 
STORE_NEXT_BLOCK_START:  
            LD      a,b 
;            LD      (NEXT_BLOCK_START_BANK),A
            LD      (IY + NEXT_BLOCK_START_BANK_OFS),A 
;            LD      (NEXT_BLOCK_START_ADDR),HL
            LD      (IY + NEXT_BLOCK_START_ADDR_OFS),L 
            LD      (IY + NEXT_BLOCK_START_ADDR_OFS+1),H 
            rs_hdr
            JR      PROCESS_PTP_BLOCK 
PTP_DATA_OR_END_BLOCK:  
;            CALL    INC2_B16K_ADDR
            RS_CALL INC2_B16K_ADDR 
;            CALL    GET_WORD_FROM_ROM ; DE contains load addr of block or exec addr of assembly
            RS_CALL GET_WORD_FROM_ROM 
            CP      PTP_NBT_BASIC_PROG 
;            JR      z,PTP_SET_DEST_ADDR_BASIC
            rs_jpz PTP_SET_DEST_ADDR_BASIC
            CP      PTP_NBT_SCREEN 
;            JR      z,PTP_SET_DEST_ADDR_SCREEN
            rs_jpz PTP_SET_DEST_ADDR_SCREEN
            CP      PTP_NBT_ASSEMBLY_PROG 
;            JR      z,COPY_PTP_BLOCK
            rs_jpz COPY_PTP_BLOCK
            CP      PTP_CBT_PROG_END 
;            JP      z,PTP_START_BASIC
            rs_jpz  PTP_START_BASIC
            CP      PTP_CBT_ASSEMBLY_END 
;            JP      z,PTP_START_ASSEMBLY
            rs_jpz  PTP_START_ASSEMBLY
INVALID_BLOCK_TYPE:  
            PUSH    DE 
            PUSH    AF 
            LD      A,$80 
            OUT     (KI4),A 
            POP     AF 
            LD      DE,$0000 ; 0. sor, 11. oszlop
            CALL    DIRCUR 
            LD      HL,(VRAM_ADDR_RAM) 
            LD      DE,$1000 + (NOT_SUPPORTED_CSTR - RAMBLOCK_ADDR) 
            ADD     HL,DE 
;            CALL    PRINT_2ND_CSTR
            RS_CALL PRINT_2ND_CSTR 
;            CALL    PRINT_HEX_8
            RS_CALL PRINT_HEX_8 
            LD      A,$20 
            CALL    DSPHND 
            POP     HL 
;            CALL    PRINT_HEX_16
            RS_CALL PRINT_HEX_16 

STOP_RIGHT_HERE:     
            JR      STOP_RIGHT_HERE 
NOT_SUPPORTED_CSTR:  
            DB      0x06 
            .CSTR   "Nem t\u007Dmogatott blokk tipus: $" 

PTP_SET_DEST_ADDR_SCREEN:
            rs_hdr
            PUSH    HL 
            LD      HL,(VRAM_ADDR_RAM) 
            ADD     HL,DE
            
            EX      DE,HL 
            POP     HL
            
;            rs_hdr
;            JR      COPY_PTP_BLOCK 
            LD      A,(VRAM_ADDR_RAM + 1) ; let's avoid to copy into this code area ($F800-)
            add     A,$0E
            CP      D ; A-D  ->  A<D -> flag c set
; TODO FIXME!!!
;            JR      nc,COPY_PTP_BLOCK
            jr      c,PTP_SKIP_BLOCK
            rs_jp COPY_PTP_BLOCK
PTP_SKIP_BLOCK:
;            CALL    INC2_B16K_ADDR
            rs_call INC2_B16K_ADDR
;            CALL    GET_BYTE_FROM_ROM
            rs_call GET_BYTE_FROM_ROM
            LD      d,0
            LD      e,a
            OR      a
            JR      nz,PTP_D_IS_SET
            INC     d
PTP_D_IS_SET:
            ADD     hl,de
            INC     hl
            INC     hl
            LD      a,h
            RLCA
            RLCA
            AND     $03
            ADD     a,b
            LD      b,a
            LD      a,h
            AND     $3F
            LD      h,a
            LD      A,B
;            LD      (NEXT_BLOCK_START_BANK),A
            ld (IY + NEXT_BLOCK_START_BANK_OFS), A
;            LD      (NEXT_BLOCK_START_ADDR),HL
            ld (IY + NEXT_BLOCK_START_ADDR_OFS), L
            ld (IY + NEXT_BLOCK_START_ADDR_OFS+1), H
;            JP      PROCESS_PTP_BLOCK
            rs_JP PROCESS_PTP_BLOCK

PTP_SET_DEST_ADDR_BASIC:
            rs_hdr
            PUSH    HL 
            LD      HL,BASIC_START_ADDRESS 
            ADD     HL,DE 
            EX      DE,HL 
            POP     HL
            rs_hdr
COPY_PTP_BLOCK:
            rs_hdr
;            CALL    INC2_B16K_ADDR
            RS_CALL INC2_B16K_ADDR 
;            CALL    GET_BYTE_FROM_ROM
            RS_CALL GET_BYTE_FROM_ROM 
            LD      C,A 
COPY_PTP_BLOCK_LOOP:  
            INC     HL 
            LD      A,H 
            CP      $40 
            JR      nz,CART_BANK_OK 
            INC     B 
            LD      A,B 
            OUT     (KI4),A 
            LD      H,0 
CART_BANK_OK:        
            LD      A,(HL) 
            LD      (DE),A 
            INC     DE 
            DEC     C 
            JR      nz,COPY_PTP_BLOCK_LOOP 
;            CALL    PTP_UPDATE_SCALAR_POI
            RS_CALL PTP_UPDATE_SCALAR_POI 
;            CALL    INC2_B16K_ADDR
            RS_CALL INC2_B16K_ADDR 
;            JP      PROCESS_PTP_BLOCK
            RS_JP   PROCESS_PTP_BLOCK 

PTP_UPDATE_SCALAR_POI:  
            RS_HDR   
;            LD      A,(BLOCK_TYPE)
            LD      A,(IY + BLOCK_TYPE_OFS) 
            CP      PTP_NBT_BASIC_PROG 
            JR      z,PTP_UPDATE_ 
            RET      
;            CP      PTP_NBT_ASSEMBLY_PROG
;            RET     nz
PTP_UPDATE_:         
            PUSH    hl 
            LD      hl,($40F9) 
            XOR     a 
            SBC     hl,de 
            POP     hl 
            JR      nc,PTP_NO_POI_UPDATE 
            LD      ($40F9),de 
            LD      ($40FB),de 
            LD      ($40FD),de 
PTP_NO_POI_UPDATE:   
            RET      

PTP_START_BASIC:
            rs_hdr
;            LD      a,(START_ROM_BANK)
            LD      A,(IY + START_ROM_BANK_OFS) 
            OUT     (KI4),A 
            LD      HL,(VRAM_ADDR_RAM) 
            LD      SP,HL 
            LD      A,(MIRROR) 
            OUT     ($00),A 
            LD      hl,$1D1E ; BASIC RUN
            PUSH    hl 
            JP      $1B5D ; INIT before BASIC, RET will continue
PTP_START_ASSEMBLY:
            rs_hdr
;            LD      a,(START_ROM_BANK)
            LD      A,(IY + START_ROM_BANK_OFS) 
            OUT     (KI4),A 
            LD      HL,(VRAM_ADDR_RAM) 
            LD      SP,HL 
            LD      A,(MIRROR)
            OR      $80
            OUT     ($00),A 
            EX      de,hl 
            JP      (hl) 

;            debug_ptp

            BLOCK_POINTER_OPS  
            PRINT_HEX  
            .ENDBLOCK  
            .DEPHASE  
END_PTP_COPY:        


START_PRI_COPY:      
            .PHASE  RAMBLOCK_ADDR 
            .BLOCK  PRI 
            POP     HL 
            LD      A,(HL) 
            LD      (IY + NEXT_BLOCK_START_BANK_OFS),A 
            LD      B,A ; B is the bank addr
            INC     HL 
            LD      E,(hl) 
            INC     HL 
            LD      D,(hl) 
            EX      DE,HL ; HL is the 16k addr within the bank
;            LD      (NEXT_BLOCK_START_ADDR),hl
            LD      (IY + NEXT_BLOCK_START_ADDR_OFS),L 
            LD      (IY + (NEXT_BLOCK_START_ADDR_OFS+1) ),h 
            RS_HDR   

PROCESS_NEXT_BLOCK:  
            RS_HDR   
;            CALL    RETRIEVE_BLOCK_PARAMETERS ; returns block type in A, addr in B-HL
            RS_CALL RETRIEVE_BLOCK_PARAMETERS 
            CP      PRI_EX_ASSEMBLY 
            JR      nz,NO_ASSEMBLY_EXEC 
;           LD      A,(START_ROM_BANK)
            LD      A,(IY + START_ROM_BANK_OFS) 
            OUT     (KI4),A 
            LD      HL,(VRAM_ADDR_RAM) 
            LD      SP,HL 
            LD      A,(IX + 3)
            AND     $FD         ; ENABLE BRK
            LD      (IX + 3),A
            LD      A,(MIRROR)
            OR      $80 ; Enable NMI
            OUT     ($00),A 
;            LD      HL,(JUMP_ADDR)
            LD      L,(IY + JUMP_ADDR_OFS) 
            LD      H,(IY + JUMP_ADDR_OFS+1) 
            JP      (HL) 
NO_ASSEMBLY_EXEC:    
            CP      PRI_EX_BASIC 
            JR      nz,NO_BASIC_EXEC 
;            LD      a,(START_ROM_BANK)
            LD      a,(IY + START_ROM_BANK_OFS) 
            OUT     (KI4),A 
            LD      hl,(VRAM_ADDR_RAM) 
            LD      SP,HL 
            LD      A,(MIRROR) 
            OUT     ($00),A 
            LD      hl,$1D1E ; BASIC RUN
            PUSH    hl 
            JP      $1B5D ; INIT before BASIC, RET will continue
; on $1D1E - interpreter
NO_BASIC_EXEC:       
;            LD      HL,(SRC_ADDR)
            LD      L,(IY + SRC_ADDR_OFS) 
            LD      H,(IY + SRC_ADDR_OFS+1) 
;            LD      DE,(BLOCK_LEN)
            LD      E,(IY + BLOCK_LEN_OFS) 
            LD      D,(IY + BLOCK_LEN_OFS+1) 
;            LD      A,(SRC_BANK)
            LD      A,(IY + SRC_BANK_OFS) 
            OUT     (KI4),A 
            LD      B,A 
            ADD     HL,DE 
            LD      A,H 
            AND     $C0 
            RLCA     
            RLCA     
            ADD     A,B 
            LD      B,A 
;            LD      (NEXT_BLOCK_START_BANK),A
            LD      (IY + NEXT_BLOCK_START_BANK_OFS),A 
            LD      A,H 
            AND     $3F 
            LD      H,A 
;            LD      (NEXT_BLOCK_START_ADDR),HL
            LD      (IY + NEXT_BLOCK_START_ADDR_OFS),L 
            LD      (IY + (NEXT_BLOCK_START_ADDR_OFS+1)),H 
; let's check if the copy block overflows of this jump code
;            LD      DE,(DEST_ADDR)
            LD      E,(IY + DEST_ADDR_OFS) 
            LD      D,(IY + DEST_ADDR_OFS+1) 

;            LD      HL,(BLOCK_LEN)
            LD      L,(iY + BLOCK_LEN_OFS) 
            LD      H,(iY + BLOCK_LEN_OFS+1) 
            ADD     HL,DE ; end of dest block in HA
            PUSH    HL 
            XOR     A ; first check if the end of block is lower then the start (overflow to $0000)
            SBC     HL,DE ; shall not cause carry!
            POP     HL 
            JR      c,AFTER_COPY ; otherwise no copy

;            LD      DE,$F800 ; check if end of block is higher than F800
            ld      DE,(VRAM_ADDR_RAM)
            push HL
            ld hl,$1000
            add hl,de
            ex de,hl
            pop hl
            XOR     A
            SBC     HL,DE ; no carry if hl > de
            jr nc,AFTER_COPY
;            CALL    c,COPY_BLOCK
            rs_CALL COPY_BLOCK

;            CALL    COPY_BLOCK
;            RS_CALL COPY_BLOCK 
AFTER_COPY:          
;            LD      HL,(NEXT_BLOCK_START_ADDR)
            LD      L,(IY + NEXT_BLOCK_START_ADDR_OFS) 
            LD      H,(IY + NEXT_BLOCK_START_ADDR_OFS+1) 
;            LD      A,(NEXT_BLOCK_START_BANK)
            LD      A,(IY + NEXT_BLOCK_START_BANK_OFS) 
            LD      B,A 

;            JP      PROCESS_NEXT_BLOCK
            RS_JP   PROCESS_NEXT_BLOCK 

RETRIEVE_BLOCK_PARAMETERS:  
            RS_HDR   
;            CALL    GET_BYTE_FROM_ROM
            RS_CALL GET_BYTE_FROM_ROM 
;            LD      (BLOCK_TYPE),A
            LD      (IY + BLOCK_TYPE_OFS),A 
            CP      PRI_EX_BASIC ; Simple BASIC execution block
            RET     z 
;CALL    INC_B16k_ADDR
            RS_CALL INC_B16k_ADDR 

;            CALL    GET_WORD_FROM_ROM ; DE: block load addr or exec addr
            RS_CALL GET_WORD_FROM_ROM 
            CP      PRI_EX_ASSEMBLY 
            JR      nz,NO_LAST_BLOCK 
            EX      de,hl 
;            LD      (JUMP_ADDR),HL
            LD      (IY + JUMP_ADDR_OFS),L 
            LD      (IY + JUMP_ADDR_OFS+1),H 
            RET      
NO_LAST_BLOCK:       
            CP      PRI_BT_ASSEMBLY 
            JR      z,DEST_ADDR_OK 
            CP      PRI_BT_SCREEN 
            JR      nz,BASIC_BLOCK 
            PUSH    HL 
            LD      HL,(VRAM_ADDR_RAM) 
            ADD     HL,DE 
            EX      DE,HL 
            POP     HL 
            JR      DEST_ADDR_OK 
BASIC_BLOCK:         
            PUSH    hl 
            LD      hl,BASIC_START_ADDRESS 
            ADD     hl,de 
            EX      de,hl 
            POP     hl 
DEST_ADDR_OK:        
;            LD      (DEST_ADDR),DE
            LD      (IY + DEST_ADDR_OFS),E 
            LD      (IY + DEST_ADDR_OFS+1),D 
;            CALL    INC2_B16k_ADDR
            RS_CALL INC2_B16k_ADDR 
;            CALL    GET_WORD_FROM_ROM
            RS_CALL GET_WORD_FROM_ROM 
;            LD      (BLOCK_LEN),DE
            LD      (IY + BLOCK_LEN_OFS),E 
            LD      (IY + BLOCK_LEN_OFS+1),D 
;            CALL    inc2_b16k_addr
            RS_CALL INC2_B16k_ADDR 
;            LD      (SRC_ADDR),HL
            LD      (IY + SRC_ADDR_OFS),L 
            LD      (IY + SRC_ADDR_OFS+1),H 
            LD      c,a 
            LD      a,b 
;            LD      (SRC_BANK),A
            LD      (IY + SRC_BANK_OFS),A 
            LD      A,C 
            RET      

COPY_BLOCK:          
            RS_HDR   
;            LD      HL,(SRC_ADDR)
            LD      L,(IY + SRC_ADDR_OFS) 
            LD      H,(IY + SRC_ADDR_OFS+1) 

;            LD      DE,(BLOCK_LEN)
            LD      E,(IY + BLOCK_LEN_OFS) 
            LD      D,(IY + BLOCK_LEN_OFS+1) 
            ADD     HL,DE 
            LD      DE,$4000 
            EX      DE,HL 
            XOR     A 
            SBC     HL,DE 
            JR      c,ONE_MORE_ROUND 
LAST_ROUND:          
;            LD      hl,(SRC_ADDR)
            LD      L,(IY + SRC_ADDR_OFS) 
            LD      H,(IY + SRC_ADDR_OFS+1) 
;            LD      a,(SRC_BANK)
            LD      A,(IY + SRC_BANK_OFS) 
            OUT     (KI4),A 
;            LD      bc,(BLOCK_LEN)
            LD      C,(IY + BLOCK_LEN_OFS) 
            LD      B,(IY + BLOCK_LEN_OFS+1) 
;            LD      de,(DEST_ADDR)
            LD      E,(IY + DEST_ADDR_OFS) 
            LD      D,(IY + DEST_ADDR_OFS+1) 
;            call debug
            LDIR     
;            CALL    UPDATE_SCALAR_POI
            RS_CALL UPDATE_SCALAR_POI 
            RET      

ONE_MORE_ROUND:      
;            LD      hl,(SRC_ADDR)
            LD      L,(IY + SRC_ADDR_OFS) 
            LD      H,(IY + SRC_ADDR_OFS+1) 
;            LD      A,(SRC_BANK)
            LD      A,(IY + SRC_BANK_OFS) 
            OUT     (KI4),A 
            LD      de,$4000 
            EX      de,hl 
            XOR     a 
            SBC     hl,de 
            PUSH    hl ; remaining bytes in the current bank
;            LD      de,(BLOCK_LEN)
            LD      E,(IY + BLOCK_LEN_OFS) 
            LD      D,(IY + BLOCK_LEN_OFS+1) 
            EX      de,hl 
            XOR     a 
            SBC     hl,de 
;            LD      (BLOCK_LEN),hl ; remaining bytes in this block
            LD      (IY + BLOCK_LEN_OFS),L 
            LD      (IY + BLOCK_LEN_OFS+1),H 
            POP     bc ; remainig bytes in the current bank
;           LD      HL,(SRC_ADDR)
            LD      L,(IY + SRC_ADDR_OFS) 
            LD      H,(IY + SRC_ADDR_OFS+1) 
; 
;           LD      DE,(DEST_ADDR)
            LD      E,(IY + DEST_ADDR_OFS) 
            LD      D,(IY + DEST_ADDR_OFS+1) 
; 
;            call debug
            LDIR     
;            LD      (DEST_ADDR),DE
            LD      (IY + DEST_ADDR_OFS),E 
            LD      (IY + DEST_ADDR_OFS+1),D 
;            LD      hl,SRC_BANK
;            INC     (hl) ; next round will start in next bank
            INC     (IY + SRC_BANK_OFS) 
            LD      hl,$0000 ; ... at the beginning of the bank
;            LD      (SRC_ADDR),hl
            LD      (IY + SRC_ADDR_OFS),L 
            LD      (IY + SRC_ADDR_OFS+1),H 
;            CALL    UPDATE_SCALAR_POI
            RS_CALL UPDATE_SCALAR_POI 
;            JP      COPY_BLOCK
            RS_JP   COPY_BLOCK 

UPDATE_SCALAR_POI:   
            RS_HDR   
;            LD      a,(BLOCK_TYPE)
            LD      A,(IY + BLOCK_TYPE_OFS) 
;            CP      PRI_BT_SCREEN
;            RET     z
            CP      PRI_BT_BASIC 
            RET     nz 
            PUSH    hl 
            LD      hl,($40F9) 
            XOR     a 
            SBC     hl,de 
            POP     hl 
            JR      nc,NO_UPDATE 
            LD      ($40F9),de 
            LD      ($40FB),de 
            LD      ($40FD),de 
NO_UPDATE:           
            RET      
            BLOCK_POINTER_OPS  
            .ENDBLOCK  
            .DEPHASE  
END_PRI_COPY:        
END_OF_ROM:          

            .ORG    START_RAMJC + (END_JUMPCODE - START_JUMPCODE) 
DRAW_POS:            
            DB      00 
WINDOW_POS:          
            DB      00 
SELECTED_POS:        
            DB      00 
;ROM_SELECTED:
;            DB      00
SELECTED_ROM_IDX:    
            DB      00 
SELECTED_ROM_BANK:   
            DB      00 

NUM_OF_ENTRIES:      
            DB      00 
TEMP_FILE_ENTRY:     
            DS      32 
TF_TYPE     EQU     TEMP_FILE_ENTRY 
TF_SIZE     EQU     TF_TYPE+1 
TF_BANK     EQU     TF_SIZE+2 
TF_ADDR     EQU     TF_BANK+1 
TF_NAME     EQU     TF_ADDR+2 
FILEIDX_DRAWN:       
            DB      00 


;            .ORG    COPYCODE_ADDR + (END_COPYCODE-START_COPYCODE)
;            .ORG    $41E8
            .ORG    RAMBLOCK_ADDR + RAMBLOCK_VARIABLES_OFSET 
NEXT_BLOCK_START_ADDR:  
            DW      $0000 
NEXT_BLOCK_START_BANK:  
            DB      $00 
SRC_ADDR:            
            DW      $0000 
SRC_BANK:            
            DB      $00 
DEST_ADDR:           
            DW      $0000 
BLOCK_LEN:           
            DW      $0000 
BLOCK_TYPE:          
            DB      $00 
JUMP_ADDR:           
            DW      $0000 
START_PRG_ROM_IDX:   
            DB      $00 
START_ROM_BANK:      
            DB      $00 

; Variable offset definitions
NEXT_BLOCK_START_ADDR_OFS EQU NEXT_BLOCK_START_ADDR - NEXT_BLOCK_START_ADDR 
NEXT_BLOCK_START_BANK_OFS EQU NEXT_BLOCK_START_BANK - NEXT_BLOCK_START_ADDR 
SRC_ADDR_OFS EQU    SRC_ADDR - NEXT_BLOCK_START_ADDR 
SRC_BANK_OFS EQU    SRC_BANK - NEXT_BLOCK_START_ADDR 
DEST_ADDR_OFS EQU   DEST_ADDR - NEXT_BLOCK_START_ADDR 
BLOCK_LEN_OFS EQU   BLOCK_LEN - NEXT_BLOCK_START_ADDR 
BLOCK_TYPE_OFS EQU  BLOCK_TYPE - NEXT_BLOCK_START_ADDR 
JUMP_ADDR_OFS EQU   JUMP_ADDR - NEXT_BLOCK_START_ADDR 
START_PRG_ROM_IDX_OFS EQU START_PRG_ROM_IDX - NEXT_BLOCK_START_ADDR 
START_ROM_BANK_OFS EQU START_ROM_BANK - NEXT_BLOCK_START_ADDR 


COPY_BLOCK_START_ADDR:  


            .ORG    $1000 ; pointers
; 2 byte pointers to list entries. Always < $4000
; last entry is 0 or at $1200
            DW      TENTRY0 
            DW      TENTRY1 
            DW      TENTRY2 
            DW      0 
            .ORG    $1200 
; 1 byte type
;   1 - pri
;   2 - ptp
;   3 - rom
; 2 byte file size (always $4000 for ROM types)
; 3 byte pointer
;   1 byte for bank
;   2 bytes for address within bank. Always < $4000 (always 0 for ROM)
; n byte - zero terminated string - menu item
TENTRY0:             
            DB      EXEC_PRI ; type pri
            DW      $000a ; 
            DB      0 
            DW      TEPAYLOAD0 ; 
            .CSTR   "Test entry #1" 
TENTRY1:             
            DB      EXEC_ROM ; type rom
            DW      $4000 ; 
            DB      2 
            DW      $0000 ; 
            .CSTR   "Test entry #2" 
TENTRY2:             
            DB      EXEC_PTP ; type ptp
            DW      $000a ; 
            DB      0 
            DW      $2000 ; 
            .CSTR   "Test entry #2" 

            .ORG    $3f80 
TEPAYLOAD0:          
            DB      $D9 
            DW      $5100 
            DW      $0102 
TELOOP:              
            JR      teloop 
            FILL    $75,$100 
            DB      $c3 
            DW      $5100 

            .ORG    $2000 
TEPAYLOAD1:          
            DB      0xff,0x10,0x00 

            DB      0x55,0x07,0x00 
            DB      0x83,0x00 
            DB      0x03 
            DB      "bob" 
            DB      0x00 ; CRC

            DB      0x55,0x0a,0x00 
            DB      0xf9,0x01 
            DW      0x5000 
            DB      0x06 
            .PHASE  $5000 
            LD      hl,$E800 
            INC     (hl) 
            JR      $5000 
            .DEPHASE  
            DB      0x01 ; CRC

            DB      0xaa,0x07,0x00 
            DB      0xb9,0x02 
            DW      0x5000 
            DB      0x01 

