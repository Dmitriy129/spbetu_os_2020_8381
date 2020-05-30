LAB3	SEGMENT
		ASSUME CS:LAB3, DS:LAB3, ES:NOTHING, SS:NOTHING
		ORG 100H 
START:	JMP BEGIN

AvailableMemory db 'Amont of available memory:        B.', 0DH,0AH, '$'
ExtendedMemory db 'Amont of extended  memory:       KB.', 0DH,0AH, '$'
Mcbline db 'New MCB:', 0DH,0AH, 'Type: $'
Mcbsector db 'h. Sector: $'
Mcbsize db 'h. Size:        B$'
LastBytes db '. Information in last bytes: $'
Endline db 0DH,0AH, '$'

BEGIN:
		mov ah, 4ah
		mov bx, 0ffffh
		int 21h
		mov ax, bx
		mov bx, 16
		mul bx
		lea si, AvailableMemory + 32
		call WRD_TO_DEC
		lea	dx, AvailableMemory
		call WRITE
		mov bx, offset LAB_END
		mov ah, 4ah
		int 21h
		mov bx, 1000h
		mov ah, 48h
		int 21h
		xor ax, ax
		xor dx, dx
		mov al, 30h
		out 70h, al
		in al, 71h
		mov bl, al
		mov al, 31h
		out 70h, al
		in al, 71h
		mov bh, al
		mov ax, bx
		lea si, ExtendedMemory + 30
		call WRD_TO_DEC
		lea	dx, ExtendedMemory
		call WRITE
		xor ax, ax
		mov ah, 52h
		int 21h
		mov cx, es:[bx-2]
		mov es, cx
mcb:
		lea	dx, Mcbline
		call WRITE
		mov al, es:[00h]
		call WRITE_BYTE
		lea dx, Mcbsector
		call WRITE
		mov ax, es:[01h]
		mov ch, ah
		mov ah, al
		mov al, ch
		call WRITE_BYTE
		mov ch, ah
		mov ah, al
		mov al, ch
		call WRITE_BYTE
		mov ax, es:[03h]
		mov bx, 10h
		mul bx
		mov si, offset Mcbsize
		add si, 14
		call WRD_TO_DEC
		mov dx, offset Mcbsize
		call WRITE
		lea dx, LastBytes
		call WRITE
		xor bx, bx
last:		
		mov dl, es:[bx+08h]
		mov ah, 02h
		int 21h
		inc bx
		cmp bx, 8
		jl last
		lea	dx, Endline
		call WRITE
		mov al, es:[00h]
		cmp al, 5Ah
		je endmcb
		xor cx, cx
		mov cx, es:[03h]
		mov bx, es
		add bx, cx
		inc bx
		mov es, bx
		jmp mcb

endmcb:
		xor al, al
		mov ah, 4Ch
		int 21h
		
WRITE_BYTE PROC near
	    push ax
	    push dx
	    push cx
	    call BYTE_TO_HEX
	    xor cx, cx
	    mov ch, ah
	    mov dl, al
	    mov ah, 02h
	    int 21h
	    mov dl, ch
	    mov ah, 02h
	    int 21h
	    pop cx
	    pop dx
	    pop ax
	    ret
WRITE_BYTE ENDP

WRITE PROC near
		mov ah, 09
		int 21h
WRITE ENDP
	
TETR_TO_HEX PROC near
        and AL,0Fh
        cmp AL,09
        jbe next
        add AL,07
next:
        add AL,30h
        ret
TETR_TO_HEX ENDP

BYTE_TO_HEX PROC near
        push CX
        mov AH,AL
        call TETR_TO_HEX
        xchg AL,AH
        mov CL,4
        shr AL,CL
        call TETR_TO_HEX 
        pop CX 
        ret
BYTE_TO_HEX ENDP

WRD_TO_DEC PROC NEAR
		push cx
		push dx
		mov cx,10
loop_b: div cx
		or dl,30h
		mov [si],dl
		dec si
		xor dx,dx
		cmp ax,10
		jae loop_b
		cmp al,00h
		je endl
		or al,30h
		mov [si],al
endl:	pop dx
		pop cx
		ret
WRD_TO_DEC ENDP

WRD_TO_HEX PROC near
        push BX
        mov BH,AH
        call BYTE_TO_HEX
        mov [DI],AH
        dec DI
        mov [DI],AL
        dec DI
        mov AL,BH
        call BYTE_TO_HEX
        mov [DI],AH
        dec DI
        mov [DI],AL
        pop BX
        ret
WRD_TO_HEX ENDP

LAB_END:
LAB3	ENDS
		END START
