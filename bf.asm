[org 0x100]

;;Start by getting command line arg, which is just a file name...
startup:
	movzx cx, byte [ds:0x80]	;Get arg length from PSP
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

	;...and save down the start of free mem, calculate mem map
	add di, 2
	mov word [proc_table], di	;;Proc table starts after file, takes up 512 bytes

	add di, 514
	mov word [tape_start], di	;;Tape memory comes after everything, uses rest of memory

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

pointer: dw 0				;;Storage for code pointer
tape_start: dw 0			;;Start of free memory for tape
file_end: dw 0				;;End of loaded file
mem_reg: db 0				;;Memory register used by cbrain extension
proc_table: dw 0			;;Pointer to proc table, used by pbrain extension
call_stack: times 32 dw 0	;;Call stack, used by pbrain extension
call_depth: dw 0			;;Current depth of call stack

loops: times 32 dw 0		;;Store loop pointers here, for now
loop_depth dw 0				;;How deep into loops are we? Used to address loops list

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
	je .not_an_op			;...then bail!

	cmp byte [si], al		;Do we have a matching char?
	je .run_inst			;...then we need to run the corresponding handler
	
	add si, 3				;Increment up to the next table entry
	jmp .loop

.not_an_op:
	inc word [pointer]
	jmp .done

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
	;;Standard brainfuck
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

	;;pbrain extension
	db '('
	dw _set_proc

	db ')'
	dw _ret_proc

	db ':'
	dw _eval_proc

	;;cbrain extension
	db '0'
	dw _num_inst

	db '1'
	dw _num_inst

	db '2'
	dw _num_inst

	db '3'
	dw _num_inst

	db '4'
	dw _num_inst

	db '5'
	dw _num_inst

	db '6'
	dw _num_inst

	db '7'
	dw _num_inst

	db '8'
	dw _num_inst

	db '9'
	dw _num_inst

	db '{'
	dw _comment_inst

	db '*'
	dw _mul_inst

	db '/'
	dw _div_inst

	db 'a'
	dw _a_inst

	db 's'
	dw _s_inst	

	db 'm'
	dw _m_inst

	db 'd'
	dw _d_inst

	db '^'
	dw _xor_inst

	db '&'
	dw _and_inst

	db '|'
	dw _or_inst

	db '~'
	dw _not_inst

	db 'r'
	dw _rshift_inst

	db 'l'
	dw _lshift_inst
	
	db '%'
	dw _mod_inst

	db '@'
	dw _store_inst

	db '!'
	dw _retv_inst

	db 'x'
	dw _xchg_inst

	db 'o'
	dw _bcopy_inst

	db 'c'
	dw _fcopy_inst

	db 'n'
	dw _int_out_inst

	db '#'
	dw _int_out_inst

	db '='
	dw _int_in_inst

	db 0

;Instructions, they are passed...
;	DI - Pointer to tape
;	AL - Instruction character
_stub_inst:
	mov dl, al
	mov ah, 0x02
	int 0x21
	ret

_inc_inst:						
	add byte [di], 1			;;Just increment cell
	add word [pointer], 1
	ret

_dec_inst:
	sub byte [di], 1			;;Decrement cell
	add word [pointer], 1
	ret

_right_inst:
	inc di						;;Increment tape pointer
	add word [pointer], 1
	ret

_left_inst:
	dec di						;;Decrement tape pointer
	add word [pointer], 1
	ret

_out_inst:
	mov dl, byte [di]
	mov ah, 0x02
	int 0x21					;;Output value of cell
	add word [pointer], 1
	ret

_in_inst:
	mov ah, 0x01
	int 0x21					;;Input keypress to cell
	mov byte [di], al
	add word [pointer], 1
	ret

_start_loop_inst:
	;;All the real smarts is handled by the end of loop instruction
	mov bx, word [loop_depth]		;;Grab loop depth
	mov si, word [pointer]			;;Grab pointer to current instruction
	mov word [loops + bx], si		;;Save address of start of loop at end of loops list
	add word [loop_depth], 2		;;We are now one level deeper, but each pointer is 1 word wide
	add word [pointer], 1			;;Increment, as usual
	ret

_end_loop_inst:
	cmp byte [di], 0				;;Are we done iterating?
	je .terminate_loop

	;;We are returning to the top
	mov si, loops					;;Start at the loops list
	add si, word [loop_depth]		;;Add depth to get to end of list
	sub si, 2						;;We are always at -1 (loops[len - 1])

	mov ax, word [si]				;;Get the start of the loop
	add ax, 1						;;Gotta advance past the [
	mov word [pointer], ax			;;Set the pointer
	ret

.terminate_loop:
	sub word [loop_depth], 2		;;We are liberated from this level
	add word [pointer], 1			;Increment as usual
	ret

;;pbrain extension
_set_proc:							;;Save procedure, named by current value of tape cell
	movzx ax, byte [di]
	mov bl, 2						;;Gotta multiply by 2 since proc table stores words
	mul bl

	mov si, word [proc_table]		;;Grab proc table location
	add si, ax						;;Increment up to given cell value
	mov bx, word [pointer]			;;Grab current code pointer
	inc bx							;;Increment so we are past the (
	mov word [si], bx				;;Save pointer over to table

	mov si, bx
.loop:								;;No we search for the matching )
	inc si
	cmp byte [si], ')'
	jne .loop

	inc si							;;Increment past the )
	mov word [pointer], si			;;Save it
	ret

_ret_proc:							;;Return from proc via address on call stack
	mov bx, word [call_depth]		;;Grab the current depth
	sub bx, 2						;;So the whole len - 1 thing
	mov ax, word [call_stack + bx]	;;Get top of call stack
	sub word [call_depth], 2		;;Sub depth by 1 word
	mov word [pointer], ax			;;Update pointer, return to caller
	ret

_eval_proc:							;;Call a proc
	;;First, lets save the pointer
	mov cx, word [pointer]			;;Grab current pointer
	add cx, 1						;;Increment past :
	
	mov bx, word [call_depth]		;;Grab depth of call stack
	mov word [call_stack + bx], cx	;;Save down return address
	add word [call_depth], 2		;;Increment depth by 1 word

	;;Now lets fetch the new pointer from the call table
	mov ax, 2						
	mul word [di]					;;Multiply current cell value by 2
	mov si, word [proc_table]		;;Get address of proc table
	add si, ax						;;Calculate specific entry
	mov ax, word [si]				;;Grab the pointer to our proc

	mov word [pointer], ax			;;This is our new pointer

	ret

;;cbrain extension
_num_inst:							;;Handle numeric data
	sub al, 48						;;No matter what we need to convert from single digit char to int

	;;Check if previous instruction was also a number
	mov si, word [pointer]			;;Grab pointer value
	mov bl, byte [si - 1]			;;Get previous instruction
	cmp bl, '0'						;;Run the comparison
	jl .single_digit
	cmp bl, '9'
	jg .single_digit

	;;If we have preceding digit then we need to mul cell by 10 and add
	mov bl, al						;;Save down digit value
	mov al, 10						;;Multiply current cell by 10
	mul byte [di]

	add al, bl						;;Add literal value
	mov byte [di], al				;;Save to current cell
	jmp .done	

.single_digit:						;;Store raw value if no preceding digit
	mov byte [di], al
.done:
	add word [pointer], 1
	ret

_comment_inst:						;;Skip between { }, this is a comment
	mov si, word [pointer]			;;Start at tape pointer
.loop:
	inc si							;;Increment to next code cell
	cmp byte [si], '}'				;;Is the comment over?
	jne .loop						;;If not, then loop

	inc si							;;Increment past }
	mov word [pointer], si			;;Update pointer
	ret

_mul_inst:
	mov al, 10						;;Multiply cell by 10
	mul byte [di]
	mov byte [di], al

	add word [pointer], 1
	ret

_div_inst:
	xor ax, ax						;;Divide cell by 10
	mov al, byte [di]
	mov dl, 10
	div dl

	mov byte [di], al

	add word [pointer], 1
	ret

_a_inst:							;;Add cell to previous cell, <
	mov al, byte [di]
	dec di
	add byte [di], al

	add word [pointer], 1
	ret

_s_inst:							;;Sub cell from previous cell, <
	mov al, byte [di]
	dec di
	sub byte [di], al

	add word [pointer], 1
	ret

_m_inst:							;;Multiply cell by previous cell, <
	mov al, byte [di]
	dec di
	mul byte [di]
	mov byte [di], al

	add word [pointer], 1
	ret

_d_inst:							;Divide previous cell by current cell, remainder to current cell
	xor ax, ax
	mov al, byte [di - 1]
	div byte [di]

	mov byte [di - 1], al
	mov byte [di], ah

	add word [pointer], 1
	ret

_xor_inst:
	mov al, byte [di]
	dec di
	xor byte [di], al	

	add word [pointer], 1
	ret

_and_inst:
	mov al, byte [di]
	dec di
	and byte [di], al	

	add word [pointer], 1
	ret

_or_inst:
	mov al, byte [di]
	dec di
	or byte [di], al	

	add word [pointer], 1
	ret

_not_inst:
	not byte [di]	
	add word [pointer], 1
	ret

_rshift_inst:
	shr byte [di], 1	

	add word [pointer], 1
	ret

_lshift_inst:
	shl byte [di], 1
	
	add word [pointer], 1
	ret

_mod_inst:						;;Modulo previous cell by current cell, <
	xor ax, ax
	mov al, byte [di - 1]
	div byte [di]

	mov byte [di - 1], ah		;;Remainder == Modulo
	dec di

	add word [pointer], 1
	ret

_store_inst:					;;Store cell value to memory register
	mov al, byte [di]
	mov byte [mem_reg], al
	add word [pointer], 1
	ret

_retv_inst:						;;Retrieve memory register value, store in current cell
	mov al, byte [mem_reg]
	mov byte [di], al
	add word [pointer], 1
	ret

_xchg_inst:						;;Exchange current cell and previous cell
	mov al, byte [di]
	mov ah, byte [di - 1]

	mov byte [di], ah
	mov byte [di - 1], al
	
	add word [pointer], 1
	ret

_bcopy_inst:					;;(o) Copy cotents of previous cell into next cell, >
	mov al, byte [di - 1]
	mov byte [di + 1], al
	inc di	

	add word [pointer], 1
	ret

_fcopy_inst:					;;(c) Copy contents of current cell into next cell, >
	mov al, byte [di]
	mov byte [di + 1], al
	inc di	

	add word [pointer], 1
	ret

_int_out_inst:					;;Print an integer to STDOUT
	xor ax, ax					;;Clear ah
	mov al, byte [di]			;;Load in cell value
	call itoa					;;Convert to string

	mov ah, 0x09				;;Print string to STDOUT
	int 0x21

	add word [pointer], 1
	ret

_int_in_inst:					;;Fetch an integer from STDIN
	call read_stdin_string
	call atoi

	mov byte [di], al

	add word [pointer], 1
	ret

;Int to dec string
;	AX - integer to convert
;	DX - converted string, $ terminated
itoa:
    pusha
    mov cx, 0
    mov bx, 10
    mov di, .t

.push:
    mov dx, 0
    div bx
    inc cx
    push dx
    test ax, ax
    jnz .push

.pop:
    pop dx
    add dl, '0'
    mov [di], dl
	inc di
    dec cx
    jnz .pop

    mov byte [di], '$'
    popa
    mov dx, .t
	ret

    .t times 8 db '$'

;Read from STDIN to internal buffer
;	SI - pointer to buffer (.buffer), as 0 terminated string
read_stdin_string:
	pusha
	mov di, .buffer
.loop:
	mov ah, 0x01
	int 0x21
	mov byte [di], al
	inc di
	cmp al, 13
	jne .loop

	mov byte [di], '$'
	popa

	mov si, .buffer
	ret

	.buffer times 64 db '$'

;Convert (small) dec string to integer
;	SI - string to convert
;	AL - converted string's value
atoi:
	push si
	push dx
	push cx
	xor ax, ax
	mov dl, 10
.loop:
	mov cl, byte [si]
	cmp cl, '$'
	je .done
	cmp cl, '0'
	jl .done
	cmp cl, '9'
	jg .done

	mov dl, 10
	mul dl						;;Multiply ax by 10, since we are onto a new number
	sub cl, '0'					;;Convert char to int
	add al, cl					;;Add new digit
	inc si						;;Increment us up and prepare to iterate!
	jmp .loop

.done:
	pop cx
	pop dx
	pop si
	ret

file_buffer:
