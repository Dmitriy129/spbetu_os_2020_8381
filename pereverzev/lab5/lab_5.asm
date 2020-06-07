ASSUME CS:_CODE, ds:_DATA, es:NOTHING, SS:_STACK
_CODE SEGMENT
ROUT PROC FAR
    jmp start

        INT_STACK dw  128 dup(0)
        SIGNATURE dw  01984h
        KEEP_IP dw  0
        KEEP_CS dw  0
        KEEP_PSP dw  0
        KEEP_SS dw  0
        KEEP_SP dw  0
        KEY_CODE db  0
        TEMP dw  0

    start:
        mov KEEP_SS, SS
        mov KEEP_SP, SP
        mov TEMP, seg ROUT
        mov SS, TEMP
        mov SP, offset INT_STACK
        add SP, 256
        push ax
        push bx
        push CX
        push dx
        push si
        push es
        push ds
        mov ax, SEG ROUT
        mov ds, ax

in 		al, 60h
		
		cmp al, 13h
		je OUT_L
		cmp al, 18h
		je OUT_A
		
		pushf
		call DWORD PTR CS:KEEP_IP
		jmp ROUT_END
	
	OUT_L:
		mov	KEY_CODE, 'L'
		jmp	DO_REQ
	OUT_A:
		mov	KEY_CODE, 'A'
		jmp	DO_REQ
   DO_REQ:
        in al, 61h
        mov ah, al
        or al, 80h
        out 61h, al
        xchg al, al
        out 61h, al
        mov al, 20h
        out 20h, al

    ADD_TO_BUFF:
        mov ah, 05h
        mov cl, KEY_CODE
        mov CH, 00h
        int 16h
        or al, al
        jz 	ROUT_END

        mov ax, 0040h
        mov es, ax
        mov ax, es:[1ah]
        mov es:[1CH], ax
        jmp ADD_TO_BUFF

    ROUT_END:
        pop ds
        pop es
        pop si
        pop dx
        pop CX
        pop bx
        pop ax
        mov SS, KEEP_SS
        mov SP, KEEP_SP
        mov al, 20h
        out 20h, al
        IRET
ROUT ENDP
LAST_BYTE_ROUT:

PRINT PROC NEAR
        push ax
        mov ah, 09h
        int 21h
        pop ax
        ret
PRINT ENDP

CHECK_ROUT PROC
		push ax
		push bx
		push si
		mov ah, 35h
		mov al, 09h
		int 21h
		mov si, offset SIGNATURE
        sub si, offset ROUT
		mov ax, es:[bx + si]
		cmp ax, SIGNATURE
		jne CHECK_ROUT_END
        mov cl, FLAG
		add cl, 1
		mov FLAG, cl

	CHECK_ROUT_END:
		pop si
		pop bx
		pop ax

	    ret
CHECK_ROUT ENDP

SET_ROUT PROC
        push ax
		push bx
		push CX
		push dx
		push es
		push ds

        mov ah, 35h
		mov al, 09h
		int 21h
        mov KEEP_IP, bx
		mov KEEP_CS, es

        mov dx, offset ROUT
        mov ax, seg ROUT
		mov ds, ax
		mov ah, 25h
		mov al, 09h
		int 21h
		pop ds

        mov dx, offset LAST_BYTE_ROUT
        add dx, 100h
		mov cl, 4h
		shr dx, cl
		inc dx
		mov ah, 31h
		int 21h
EXIT:
        pop es
		pop dx
		pop CX
		pop bx
		pop ax

	    ret
SET_ROUT        ENDP



DELETE_ROUT      PROC
		push ax
		push bx
		push dx
		push ds
		push es
		push si

		mov ah, 35h
		mov al, 09h
		int 21h

		mov si, offset KEEP_IP
        sub si, offset ROUT
		mov dx, es:[bx + si]
		mov ax, es:[bx + si+2]

		push ds
		mov ds, ax
		mov ah, 25h
		mov al, 09h
		int 21h
		pop ds

        mov si, offset KEEP_PSP
        sub si, offset ROUT
        mov ax, es:[bx + si]
        mov es, ax
        push es
        mov ax, es:[2CH]
		mov es, ax
		mov ah, 49h
		int 21h

        pop es
        mov ah, 49h
		int 21h

        pop si
		pop es
		pop ds
		pop dx
		pop bx
		pop ax

		sti
	    ret
DELETE_ROUT      ENDP

MAIN PROC
		push ds
		xor ax, ax
		push ax
		mov ax, _DATA
		mov ds, ax
        mov KEEP_PSP, es

        mov FLAG, 0

        push ax
		cmp byte ptr es:[82h], '/'
		jne END_DELETE
		cmp byte ptr es:[83h], 'u'
		jne END_DELETE
		cmp byte ptr es:[84h], 'n'
		jne END_DELETE
		mov al, FLAG
		add al, 2
		mov FLAG, al

	END_DELETE:
		pop ax

	    call CHECK_ROUT

		cmp FLAG, 0
		je INT_LOAD
		cmp FLAG, 1
		je INT_LOAD_AGAIN
		cmp FLAG, 2
		je INT_UNLOAD_AGAIN
		jmp INT_UNLOAD

	INT_LOAD:
        mov dx, offset LOADED
        call PRINT
		call SET_ROUT
		jmp MAIN_END

	INT_LOAD_AGAIN:
		mov dx, offset ALREADYLOADED
        call PRINT
		jmp MAIN_END

	INT_UNLOAD_AGAIN:
		mov dx, offset NOTLOADED
		call PRINT
		jmp MAIN_END

	INT_UNLOAD:
		call DELETE_ROUT
        mov dx, offset UNLOADED
		call PRINT
		jmp MAIN_END

	MAIN_END:
		mov ax, 4C00h
	    int 21h
	MAIN ENDP

_CODE ENDS

_STACK SEGMENT STACK
		dw  128 dup(0)
_STACK ENDS

_DATA SEGMENT
    LOADED db 'Was loaded!', 0dh, 0ah, '$'
	UNLOADED db 'Was unloaded!', 0dh, 0ah, '$'
	ALREADYLOADED db 'Already loaded!', 0dh, 0ah, '$'
	NOTLOADED db 'Was not loaded!', 0DH, 0ah, '$'
    FLAG db  0;
_DATA ENDS

END MAIN