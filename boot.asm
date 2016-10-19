	BITS 16

start:
	mov ax, 07C0h		; Set up 4K stack space after this bootloader
	add ax, 288		; (4096 + 512) / 16 bytes per paragraph
	mov ss, ax
	mov sp, 4096

	mov ax, 07C0h		; Set data segment to where we're loaded
	mov ds, ax


	;mov si, text_string	; Put string position into SI
	;call print_string	; Call our string-printing routine
	call suma

	jmp $			; Jump here - infinite loop!

	msg1     db      'N1: ', 0
	msg2     db      'N2: ', 0 
	msg3     db      'Sum: ', 0
	input1 resb 5
	input2 resb 5
	sum resb 5

	ent1 resb 4
	ent2 resb 4
	result resb 4

os_get_cursor_pos:
	pusha

	mov bh, 0
	mov ah, 3
	int 10h				; BIOS interrupt to get cursor position

	mov [.tmp], dx
	popa
	mov dx, [.tmp]
	ret


	.tmp dw 0

reset_cursor:
	call os_get_cursor_pos
	inc dh
	mov dl, 0
	mov bh, 0
	mov ah, 2
	int 10h	
	ret

get_input_string:
	;ax -> mem address to store the input
	push bp
	mov bp, sp
	sub sp, 8 ; 8 bytes for local vars

	mov word[bp-4], ax ;mem addres
	mov word[bp-8], 0 ; next position to write

	while: ;<4
	
	cmp word[bp-8], 4
	jz .finish

	mov ah,0h   ;get character from keyboard
	int 16h     ;and store it in AL

	cmp al, 0x0D
	jz .finish



	mov cx, word[bp - 8]
	mov bx, word[bp - 4]
	add bx, cx
	mov byte[bx], al
	mov ah, 0Eh
	int 10h

	inc word[bp-8]

	jmp while

.finish:
	add sp, 8
	pop bp
	ret

pow:
	;ax -> base
	;bx -> exponent
	;ax -> result

	mov cx, 0 ; iterator

	cmp bx, 0
	jz ret_1
	cmp bx, 1
	jz pow_finish
	mov cx, 2
	
	while_pow:
	cmp cx, bx
	jg pow_finish

	mul ax
	inc cx
	jmp while_pow

ret_1:
	mov ax, 1
	jmp pow_finish

pow_finish:
	ret

strlen:
	;ax -> mem address that contain the string
	;ax -> length of string

	mov cx, 0
	while_strlen:
		mov bx, ax
		add bx, cx
		mov dh, byte[bx]
		cmp dh, 0
		jz .strlen_finish
		inc cx
		jmp while_strlen

	.strlen_finish:
		mov ax, cx
		ret


atoi:
	;ax -> mem address that contain the string
	;ax -> integer

	push bp
	mov bp, sp
	sub sp, 8

	mov word[bp-2], ax ; mem address
	mov word[bp-4], 0 ; length
	mov word[bp-6], 0 ; integer to return
	mov dword[bp-8], 0 ; iterator

	call strlen 
	mov word[bp-4], ax

	while_atoi:
		mov ax, word[bp-8]
		mov bx, word[bp-4]
		cmp ax, bx
		jz .finish_atoi

		mov ax, 10
		mov bx, word[bp-4]
		sub bx, 1
		mov cx, word[bp-8]
		sub bx, cx
		call pow	

		mov cx, ax

		mov ax, word[bp-2] ; mem address
		mov bx, word[bp-8] ; legnth
		add bx, ax
		mov al, byte[bx]

		sub al, 48

		mul cx

		add word[bp-6], ax

		inc word[bp-8]

		jmp while_atoi


	.finish_atoi:
		mov ax, word[bp-6]
		add sp, 8
		pop bp
		ret

itoa:
	;ax -> mem address that contain the integer
	;bx -> mem address to store the string

	push bp
	mov bp, sp
	sub sp, 8

	mov bx, ax
	mov ax, [bx]

	mov [bp-2], ax ; mem address
	mov word[bp-4], 0 ; max
	mov [bp-6], bx ; mem address to store
	mov word[bp-8], 0 ; iterator

	while_less_than:
		mov ax, 10 ;base
		mov bx, [bp-4] ; potencia
		call pow
		mov bx, [bp-2]
		cmp ax, [bx]
		jg .continue
		inc word[bp-4]
		jmp while_less_than

	.continue:

	dec word[bp-4]
	
	while_store:

		cmp word[bp-4], 0
		jl .salir

		mov ax, 10
		mov bx, [bp-4]
		call pow

		mov bx, ax
		mov ax, [bp-2]
		div bx

		mov [bp-2], dx

		add ax, 48
		mov bx, [bp-6]
		add bx, [bp-8]
		mov [bx], ax
		inc word[bp-8]
		dec word[bp-4]
		jmp while_store

	.salir:
	mov sp, bp
	pop bp
	ret



suma:
	;print Number1
    mov si, msg1
    call print_string

    ;get n1
    mov ax, input1
    call get_input_string

    call reset_cursor

    mov ax, input1
    call atoi

    mov [ent1], ax
    mov ax, ent1
    mov bx, input1
    call itoa

    mov si, input1
    call print_string


	;mov si, input1
	;call print_string
	;mov ah,0eh  ;Display a character in AL
	;int 10h     ;aka, echo int
    ;mov di, input1
    ;call os_input_string

    ;print Number2
    call reset_cursor
    mov si, msg2
    call print_string

    ;get n2
    mov ax, input2
    call get_input_string
	;mov si, input2
	;call print_string
	;mov ah,0eh  ;Display a character in AL
	;int 10h     ;aka, echo int
	
    ;mov di, input2
    ;call os_input_string

    ;print Sum
    call reset_cursor
    mov si, msg3
    call print_string

    ;sum
    mov ax, [input1]
    sub ax, 48
    mov bx, [input2]
    sub bx, 48
    add ax, bx
    add ax, 48
    mov [sum], ax

    ;print the result
    mov si, sum
    call print_string

    call reset_cursor

    mov ax, [sum]
    cmp ax, 48
    jnz suma

    ret


print_string:			; Routine: output string in SI to screen
	mov ah, 0Eh		; int 10h 'print char' function

.repeat:
	lodsb			; Get character from string
	cmp al, 0
	je .done		; If char is zero, end of string
	int 10h			; Otherwise, print it
	jmp .repeat

.done:
	ret


	times 510-($-$$) db 0	; Pad remainder of boot sector with 0s
	dw 0xAA55		; The standard PC boot signature


	;%include 'input.asm'
