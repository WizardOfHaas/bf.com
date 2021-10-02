[org 0x100]

;;Start by getting command line arg, which is just a file name...
startup:
	movzx cx, byte ds:[0x80]	;Get arg length from PSP
	mov dx, 0x82				;Point to arg + 1 byte (since there is a space)

	mov di, dx
	add di, cx
	dec di

	mov byte [di], 0

	;mov dx, test_file			;;For testing, until I get cmd line args working...
	
	mov ah, 0x3d
	mov al, 0
	int 0x21
	jc .file_err

	mov bx, ax					;Set file handle
	mov ah, 0x3F				;We are going to read the file
	mov dx, file_buffer			;Buffer to store file contents in
	mov cx, 1024
	int 0x21
	jc .file_err

	;We need to terminate the file string
	mov di, dx
	add di, ax
	mov byte [di], 0

	;Save the end of the file
	dec di
	mov word [file_end], di

	;...and save down the start of free mem
	add di, 2
	mov word [tape_start], di

	;Point to the file we just loaded and let it rip!
	mov si, file_buffer
	call execute_bf

	int 0x20

.file_err:
	mov dx, _file_err_msg
	mov ah, 0x09
	int 0x21
	int 0x20 

_file_err_msg: db 'File cannot be read!', 10, 13, '$'

test_file: db "test.bf", 0

pointer: dw 0
tape_start: dw 0
file_end: dw 0

;Execute some brainfuck code
;	SI - zero terminated string of code
execute_bf:
	mov di, word [tape_start];We will always be pointing to SOMEWHERE on the tape
	mov word [pointer], si	 ;Save the start of our program to memory
	
.loop:
	cmp si, word [file_end]
	je .done

	mov al, byte [si]
	cmp al, 0				;Are we terminated?
	je .done

	call run_bf_inst
	jmp .loop

.done:
	ret

newline:
	pusha
	mov dl, 10
	mov ah, 0x02
	int 0x21
	mov dl, 13
	mov ah, 0x02
	int 0x21
	popa
	ret

;This is our biiiiig switch/case for actually running each instruction
;	AL - instruction character
run_bf_inst:
	mov word [pointer], si	

	push si
	mov si, instructions	;Start at the instruction call table
.loop:
	cmp byte [si], 0		;Are we at the end of the table??
	je .done				;...then bail!

	cmp byte [si], al		;Do we have a matching char?
	je .run_inst			;...then we need to run the corresponding handler
	
	add si, 3				;Increment up to the next table entry
	jmp .loop
.run_inst:
	call word [si + 1]
.done:
	pop si

	mov si, word [pointer]
	ret

;Instruction table
; formatted as...
;	char(byte)|handler(word)
instructions:
	db '+' 
	dw _inc_inst

	db '-'
	dw _dec_inst

	db '>'
	dw _right_inst

	db '<'
	dw _left_inst

	db '.'
	dw _out_inst

	db ','
	dw _in_inst

	db '['
	dw _start_loop_inst

	db ']'
	dw _end_loop_inst
	db 0

_stub_inst:
	mov dl, al
	mov ah, 0x02
	int 0x21
	ret

_inc_inst:
	add byte [di], 1
	add word [pointer], 1
	ret

_dec_inst:
	sub byte [di], 1
	add word [pointer], 1
	ret

_right_inst:
	inc di
	add word [pointer], 1
	ret

_left_inst:
	dec di
	add word [pointer], 1
	ret

_out_inst:
	mov dl, byte [di]
	mov ah, 0x02
	int 0x21
	add word [pointer], 1
	ret

_in_inst:
	mov ah, 0x01
	int 0x21
	mov byte [di], al
	add word [pointer], 1
	ret

_start_loop_inst:
	cmp byte [di], 0
	je .jump_forward
	add word [pointer], 1
	ret

.jump_forward:
	mov si, word [pointer]

.search_loop:
	lodsb
	cmp al, ']'
	jne .search_loop

	inc si	
	mov word [pointer], si
	ret

_end_loop_inst:
	cmp byte [di], 0
	jne .jump_backwards
	add word [pointer], 1
	ret

.jump_backwards:
	mov si, word [pointer]

.search_loop:
	dec si
	cmp byte [si], '['
	jne .search_loop

	mov word [pointer], si
	ret

file_buffer:
