[org 0x0100]

jmp start

s:						db	'SCORE:'
pop_up:					db	'Press any key to Start Game..!'
poison:					db	'*'

;--------------------------------
;these are the characters to be
;placed on the boundries of the 
;board
upper_line:				db	0xC4
lower_line:				db 	0xC4
right_line:				db 	0xB3
left_line:				db 	0xB3
upper_rigth:			db 	0xBF
upper_left:				db 	0xDA
lower_right:			db 	0xD9
lower_left:				db 	0xC0
;--------------------------------

score:					dw 	0
snake:					db 	254
snake_head:				db  229
snakelen:				dw 	5
boundpos:				times 210 dw 0

food_pos:				dw 0

snake_pos:				dw 1020, 1022, 1024, 1026, 1028, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ;maximum length of snake is 250
tempsnake_pos:			dw 1020, 1022, 1024, 1026, 1028, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
last_snake_state:  		times 50 dw 0

;-----------------------------------------
;OLD ISR value for the KEYBOARD interrupt
oldisr: 				dd 0

;-------------------------------------------------------------------------------------------------
;these are conditions of directions in which the snake is allowed to go after certain move
up: 					dw 1  				; states in which snake can go 
down: 					dw 1 				; at initial stage snake can go up ,down and forward 
forw: 					dw 1
rev: 					dw 0
;-------------------------------------------------------------------------------------------------
;these are the conditions which are specifying the current movement of SNAKE
uflag: 					dw 0 			; UP	  flag
dflag: 					dw 0 			; DOWN 	  flag
fflag: 					dw 0 			; FORWARD flag
rflag: 					dw 0 			; REVERSE flag
exflag: 				dw 0
;-------------------------------------------------------------------------------------------------

fruit: 					db 'F'

delay: 					dw 40000

game_over: db 'GAME OVER :('

liv:	   db 'Lives (out of 3): '
lives:	   dw 3

;These are the variables for TIME calculation
oldTimer: dd 0

tickcount: dw 0

second: dw 0
minutes: dw 0

seconds_remaining: dw 60
minutes_remaining: dw 3

time_gone: db 'Time Played: '
rem_time: db 'Remaining Time: '

time_over: db 0
t_over: db 'Time over :('

timing_set: db '   :   '

make_poison: dw 0

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


;This routine will return the random number in 'dx'
randomNum:
    push ax
    push bx
    push cx

    ;Random Number Generation
    ;-------------------------------------------------------------------
    mov ah, 0
    int 0x1A ;CX:DX = number of clock ticks since midnight
    ;-------------------------------------------------------------------
    ;saving values in proper registers for division
    xchg bx, cx ;saving value of 'cx' for 'dx'
    xchg ax, dx ;swapping 'ax' with 'dx'
    xchg dx, bx ;restoring value of 'cx' for 'dx'

    mov bx, 2720  ;divisor(18*160) to get random value

    div bx      ;now we can get "remainder in dx" and "quotient in ax"

    mov bx, dx  ;saving remainder
    and bx, 1   ;mod with 2

    cmp bx, 0
    jz no_use

    inc dx

    no_use:
    add dx, 1120

    ;now dx contains the random number in range of '0' to '80'
    ;-------------------------------------------------------------------

    pop cx
    pop bx
    pop ax
    ret

    gen_foo:
        push bx
        food_posX:
        call randomNum
        mov bx, dx

        and bx, 157 ;mod with 158
        cmp bx, 0
        jz food_posX

        mov bx, dx
        and bx, 159 ;mod with 160
        cmp bx, 0
        jz food_posX

		mov word[food_pos], dx

        pop bx
        ret


	make_food_random:
		pusha

		mov cx, word[snakelen]
		xor ax, ax
		mov bx, 0
		adding_snake:
			add ax, word[snake_pos+bx]
        	add bx, 2
			loop adding_snake

		add ax, word[food_pos]
		and ax, 2721
		add ax, 1120

		mov cx, ax
		and cx, 159
		cmp cx, 0
		jnz next_checkX
		sub ax, 6
		mov word[make_poison], 1

		next_checkX:
		mov cx, ax
		and cx, 957
		cmp cx, 0
		jnz work_done
		sub ax, 10
		mov word[make_poison], 1

		work_done:
		mov word[food_pos], ax

		popa
		ret


;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Part of BEEP generating
make_beep:
    push bp
    mov bp, sp
    push ax

    mov al, 182
    out 0x43, al
    mov ax, [bp+4]
    out 0x42, al

    mov al, ah
    out 0x42, al

    in al, 0x61
    or al, 0x03

    out 0x61, al

    mov cx, 0xFFFF
    looping_beep:
    push ax
    pop ax
    push ax
    pop ax
    push ax
    pop ax
    push ax
    pop ax
    push ax
    pop ax
    push ax
    pop ax
    push ax
    pop ax
    push ax
    pop ax
    loop looping_beep

    mov cx, 0xFFFF
    looping_beep_:
    loop looping_beep_

    in al, 0x61

    and al, 0xFC
    out 0x61, al

    pop ax
    pop bp

    ret 2

;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Part of TIMER HOOKING, PRINTING TIME on different positions on the SCREEN
print_timer:
    push bp
    mov bp, sp

    push es
    push ax
    push bx
    push cx
    push dx
    push di

    mov ax, 0xb800
    mov es, ax              ; point es to video base

    mov ax, [bp+6]          ; load number in ax
    mov bx, 10              ; use base 10 for division
    mov cx, 0               ; initialize count of digits

    nextdigit:
        mov dx, 0           ; zero upper half of dividend
        div bx              ; divide by 10
        add dl, 0x30        ; convert digit into ascii value
        push dx             ; save ascii value on stack
        inc cx              ; increment count of values
        cmp ax, 0           ; is the quotient zero
        jnz nextdigit       ; if no divide it again

    mov di, word[bp+4]      ; point di to 70th column

    nextpos:
        pop dx              ; remove a digit from the stack
        mov dh, 0x07        ; use normal attribute
        mov [es:di], dx     ; print char on screen
        add di, 2           ; move to next screen location
        loop nextpos        ; repeat for all digits on stack

    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    pop es

    mov sp, bp
    pop bp

    ret 4

; timer interrupt service routine
timer:
    pusha
    inc word[cs:tickcount]

	;if 4 minutes gone
	cmp word[minutes_remaining], 0
	jnz nothing
	mov byte[time_over], 1
	add word[score], 200

	nothing:
    cmp word[cs:tickcount], 18
    jne exit_timer

    inc word[cs:second]

	;finding if 20 seconds passed
	mov dx, word[cs:second]
	and dx, 19
	cmp dx, 0
	jnz no_need
	cmp word[delay], 0
	jle no_need
	shr word[delay], 1

	no_need:
    dec word[cs:seconds_remaining]
    mov word[cs:tickcount], 0

    mov ax, word[cs:second]
    cmp ax, 60
    jne exit_timer
    mov word[cs:second], 0
    mov word[cs:seconds_remaining], 60
    inc word[cs:minutes]
    dec word[cs:minutes_remaining]

    exit_timer:
        ;------------------------------
        ;Printing Clock
        ;------------------------------
		push time_gone
		push 16
		push 114
		mov ax, 0x07
		push ax
		call printstr

		push timing_set
		push 7
		push 138
		mov ax, 0x07
		push ax
		call printstr

		;------------------------------
        push word[cs:minutes]
        mov ax, 140
        push ax
        call print_timer
        ;------------------------------
        push word[cs:second]
        mov ax, 148
        push ax
        call print_timer
        ;------------------------------



        ;------------------------------
        ;Reverse Printing Clock
        ;------------------------------
		push rem_time
		push 16
		push 268
		mov ax, 0x07
		push ax
		call printstr

		push timing_set
		push 7
		push 298
		mov ax, 0x07
		push ax
		call printstr

		;------------------------------
        mov ax, 300
        push word[cs:minutes_remaining]
        push ax
        call print_timer
        ;------------------------------
        push word[cs:seconds_remaining]
        mov ax, 308
        push ax
        call print_timer
        ;------------------------------



        xor ax, ax
        mov al, 0x20
        out 0x20, al                ; end of interrupt
        popa
        iret            ; return from interrupt



;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;Part of KEYBOARD HOOKING, PRINTING SNAKE, moving SNAKE, SCORE PRINTING and handling all the checks
kbisr:
	cli

	in al,0x60					;taking the input from the input port

	cmp al,01
	jnz forward_cmp
	
	mov word[fflag], 0
	mov word[uflag], 0
	mov word[rflag], 0
	mov word[dflag], 0
	mov word[exflag], 1
	jmp kb_exit
	
	;making the SNAKE move forward
	forward_cmp:
		cmp al,0x4b
		jnz up_cmp
		cmp word[forw], 1
		jnz ending1
		
		mov word[fflag], 1
		mov word[uflag], 0
		mov word[rflag], 0
		mov word[dflag], 0
		
		mov word[up], 1
		mov word[down], 1
		mov word[forw], 1
		mov word[rev], 0
		ending1: 				;intermediate jump so that jump don't get out of range(HAPPENED IN ASSIGNMENT 3)
			jmp kb_exit
		
	;making the SNAKE move UP
	up_cmp:
		cmp al,0x48
		jnz down_cmp
		cmp word[up],1
		jnz ending2
		
		mov word[fflag],0
		mov word[uflag],1
		mov word[rflag],0
		mov word[dflag],0

		mov word[up],1
		mov word[down],0
		mov word[forw],1
		mov word[rev],1
		ending2:				;intermediate jump so that jump don't get out of range
			jmp kb_exit

	;making the SNAKE move DOWN
	down_cmp:
		cmp al,0x50
		jnz right_cmp
		cmp word[down],1
		jnz right_cmp
		
		mov word[fflag],0
		mov word[uflag],0
		mov word[rflag],0
		mov word[dflag],1

		mov word[up],0
		mov word[down],1
		mov word[forw],1
		mov word[rev],1

	;making the SNAKE move right
	right_cmp:
		cmp al,0x4D
		jnz kb_exit
		cmp word[rev],1
		jnz kb_exit
		
		mov word[fflag],0
		mov word[uflag],0
		mov word[rflag],1
		mov word[dflag],0

		mov word[up],1
		mov word[down],1
		mov word[forw],0
		mov word[rev],1

	kb_exit:
		sti
		mov al, 0x20
		out 0x20, al ; send EOI to PIC
		iret


;Score printing
printnum:
	push bp
	mov bp,sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di

	mov ax, 0xb800
	mov es, ax

	mov bx, 16
	mov cx, 0
	mov ax, [bp+4]

	nextdigit_printing:
		mov dx, 0
		div bx
		add dl, 0x30
		push dx
		inc cx
		cmp ax, 0
		jnz nextdigit_printing

	mov di, 14

	nextpos_of_printing:
		pop dx
		mov dh,0x07
		mov [es:di],dx
		add di,2
		loop nextpos_of_printing

	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es 
	pop bp
	ret 2

;-----------------------------------------
;We are using this function to print SNAKE
printstr:
	push bp
	mov bp,sp
	push es
	push ax
	push cx
	push si
	push di

	mov ax,0xb800
	mov es,ax

	mov di, [bp+6]
	mov si, [bp+10]
	mov cx, [bp+8]
	mov ax, [bp+4]

	mov bh, al
	mov ah, bh

	nextchar:
		mov al,[si]
		mov [es:di],ax
		add si,1
		add di,2
		loop nextchar

	pop di
	pop si
	pop cx 
	pop ax
	pop es
	pop bp

	ret 8
;-----------------------------------------

print_message:
	push bp
	mov bp,sp

	push es
	push ax
	push cx
	push si
	push di

	mov ax, 0xb800
	mov es, ax
	mov di, [bp+4]	;position
	mov si, [bp+8]	;offset
	mov cx, [bp+6]	;length
	mov ah, 0x73

	nextch:
		mov al,[si]
		mov [es:di],ax
		add si,1
		add di,2
		loop nextch

	pop di
	pop si
	pop cx 
	pop ax
	pop es
	pop bp

	ret 6

;----------------------------------------------------------------------------------
clrscr:
	push es
	push ax
	push di

	mov ax, 0xb800
	mov es, ax
	mov di, 0
	next:
	mov word[es:di], 0x0720
	add di, 2
	cmp di, 4000
	jne next

	mov di, 960
    mov cx, 80
    mov dx, 18
    outer_loop:
        looper:
            mov word[es:di], 0x7320 ;making board of beige color
            add di, 2               ;increment in character
            loop looper             ;board printing loop
        ;add di, 20      ; jumping to correct position on next line
        mov cx, 80      ; because there are 80 characters in board
        dec dx          ; because there are 18 rows in the board

        cmp dx, 0       ; checking when we can end the outer loop
        jnz outer_loop  ; repeating outer loop

	pop di
	pop ax 
	pop es 
	ret

;----------------------------------------------------------------------------------
;Printing boundry lines
print_boundry:
	;Saving registers in memory
	push ax
	push es
	push di

    xor ax, ax
    mov ax, 0xb800
    mov es, ax	;pointing es to video memory

	;--------------------------------------
	;upper row - line
	mov di, 800
	.upper_row:
		mov word[es:di], 0x07C4
		add di, 2
		cmp di, 960		;808+142
		jne .upper_row

	mov di, 3840
	.lower_boundry:
		mov word[es:di], 0x07C4
		add di, 2
		cmp di, 4000
		jne .lower_boundry

	mov di, 3840
	mov word[es:di], 0x07C0

	mov di, 3998
	mov word[es:di], 0x07D9

	mov di, 800
	mov word[es:di], 0x07DA

	mov di, 958
	mov word[es:di], 0x07BF

	;--------------------------------------
	;Clearing the registers
	pop di
	pop es
	pop ax

	ret

;-----------------------------------------
;We are using this function to print SNAKE
s_p:	;this is being used only for the printing of SNAKE
	push bp
	mov bp,sp
	push es
	push ax
	push cx
	push si
	push di

	mov ax,0xb800
	mov es,ax

	mov di,[bp+4]
	mov si,[bp+8]
	mov cx,[bp+6]

	mov ah,0x74

	nextcharX:
	mov al,[si]
	mov [es:di],ax
	add si,1
	add di,2
	loop nextcharX

	pop di
	pop si
	pop cx 
	pop ax
	pop es
	pop bp

	ret 6
;-----------------------------------------

;----------------------------------------------------------------------------------
;This is being used in clearing the footprints of snake :)
;Simply by drawing the board again.
regenerate_board:
	pusha
	push di

	mov ax, 0xb800
	mov es, ax

	mov di, 960
    mov cx, 80
    mov dx, 18
    outer_loopX:
        looperX:
            mov word[es:di], 0x7320 ;making board of beige color
            add di, 2               ;increment in character
            loop looperX            ;board printing loop
        ;add di, 20      ; jumping to correct position on next line
        mov cx, 80      ; because there are 80 characters in board
        dec dx          ; because there are 18 rows in the board

        cmp dx, 0       ; checking when we can end the outer loop
        jnz outer_loopX  ; repeating outer loop

	
	mov di, 960
	mov cx, 18
	left_b:
	mov word[es:di], 0x07B3
	add di, 160
	loop left_b

	mov di, 1118
	mov cx, 18
	right_b:
	mov word[es:di], 0x07B3
	add di, 160
	loop right_b

	pop di
	popa
	ret

play_game:
	pusha
	push si
	push di
	push cs

	call regenerate_board
	
	cmp byte[time_over], 1
	jz exit

	;Printing remaining lives
	push liv
	push 18
	push 162
	mov ax, 0x07
	push ax
	call printstr

	push word[cs:lives]
	mov ax, 198
    push ax
	call print_timer

	;-----------------------------
	;Checking if the snake touches
	;its own body
	xor bx, bx
	xor ax, ax

	mov bx, 2
	mov cx, [snakelen]
	sub cx, 1

	dead:
		mov ax,[snake_pos]
		cmp ax,[snake_pos+bx]
		jz exit
		add bx,2
		loop dead

	;-----------------------------
	mov ax, [snake_pos]
	mov dx, [food_pos]

	;-------------------------------------------
	;checking if the SNAKE has eaten FOOD or not
	cmp ax, dx
	jnz further

	push ax
	mov ax, 0xF8F8
    push ax
    call make_beep
	pop ax

	add word[score], 20
	add word[snakelen], 1

	call make_food_random

	further:
		push s
		push 6
		push 2
		mov ax, 0x07
		push ax
		call printstr

		mov ax,[score]
		push ax
		call printnum
		cmp word[exflag], 1
		jz end_p

	;printing food
	cmp word[make_poison], 1
	jnz posi
	push poison
	jmp posi1
	posi:
	push fruit
	posi1:
	push 1
	push word[food_pos]
	mov ax, 0xf5
	push ax

	call printstr

	mov word[make_poison], 0

	;-----------------------------------------------
	;printing snake
	mov cx, [snakelen]
	mov bx, 0

	push snake_head
	push 1
	mov ax, [snake_pos+bx]
	mov [last_snake_state+bx], ax
	push ax
	call s_p

	add bx,2

	dec cx

	printsnake:
		push snake
		push 1
		mov ax, [snake_pos+bx]
		mov [last_snake_state+bx], ax
		push ax

		call s_p
		add bx,2
		loop printsnake

		;condition to move snake forward
		cmp word[fflag], 1
		jnz upcmp
		
		;forward code
		mov bx, 0
		mov si, 2
		mov cx, [snakelen]
		forloop:
			mov ax, [cs:snake_pos+bx]
			mov [cs:tempsnake_pos+si], ax
			add bx, 2
			add si, 2
			loop forloop

		sub word[cs:tempsnake_pos], 2
		jmp array


		upcmp:
			cmp word[uflag], 1
			jnz downcmp

			;up code
			mov bx, 0
			mov si, 2
			mov cx, [snakelen]
			uploop:
				mov ax, [cs:snake_pos+bx]
				mov [cs:tempsnake_pos+si], ax
				add bx, 2
				add si, 2
				loop uploop

			sub word[cs:tempsnake_pos], 160
			jmp array

		downcmp:
			cmp word[dflag], 1
			jnz revcmp

			;down code
			mov bx, 0
			mov si, 2
			mov cx, [snakelen]

			downloop:
				mov ax, [cs:snake_pos+bx]
				mov [cs:tempsnake_pos+si], ax
				add bx, 2
				add si, 2
				loop downloop

			add word[cs:tempsnake_pos], 160
			jmp array

		revcmp:
			cmp word[rflag], 1
			jnz array

			;reverse code
			mov bx, 0
			mov si, 2
			mov cx, [snakelen]
			revloop:
			mov ax, [cs:snake_pos+bx]
			mov [cs:tempsnake_pos+si], ax

			add bx, 2
			add si, 2
			loop revloop
			add word[cs:tempsnake_pos], 2

		;making SNAKE travel through the 
		array:
			mov bx, 0
			mov si, 0
			mov cx, [snakelen]
			arraycopy:
				mov ax, [cs:tempsnake_pos+bx]
				mov [cs:snake_pos+si], ax
				add bx, 2
				add si, 2
				loop arraycopy

			;--------------------------------------------
			; right check
			mov di, [snake_pos]
			add di, 2
			mov ax, di
			xor dx, dx
			mov cx, 160
			div cx
			cmp dx, 0
			je ending_h
			
			; left check
			mov di, [snake_pos]
			mov ax, di
			xor dx, dx
			mov cx, 160
			div cx
			cmp dx, 0
			je ending_h

			; upper bound check
			mov di, [snake_pos]
			cmp di, 960
			jbe ending_h

			; lower bound check
			mov di, [snake_pos]
			cmp di, 3840
			ja ending_h

			jmp h_
			ending_h:
				jmp exit


			;--------------------------------------------

		h_:
			;Making delay to slow down the snake
			mov cx, [delay]
			delayloop1:
				push ax
				pop ax
				push ax
				pop ax
				push ax
				pop ax
				loop delayloop1

			mov cx, [delay]
			delayloop2:
				push ax
				pop ax
				push ax
				pop ax
				push ax
				pop ax
				loop delayloop2

		jmp play_game

		end_p:
			pop cs
			pop di
			pop si
			popa

			ret

start: 
	call clrscr

	push pop_up
	push 30
	push 690
	push 0x07
	call printstr

	; Waiting for a single key stroke
	mov ah, 0
	int 16h

	;call clrscr

	;_____________________________________________________
	;Here we are saving the old value of KEYBOARD ISR
	xor ax,ax
	mov es,ax	;pointing es to IVT

	mov ax, [es:0x9*4]
	mov [oldisr], ax
	mov ax, [es:0x9*4+2]
	mov [oldisr+2], ax

	;HOOKING KEYBOARD HARDWARE INTERRUPT
	;-----------------------------------
	cli        						; disable interrupts
	mov word[es:0x9*4], kbisr		; store offset at n*4
	mov word[es:0x9*4+2], cs		; store segment at n*4+2
	sti								; enable interrupts
	;-----------------------------------
	;_____________________________________________________

	;HOOKING THE TIMER INTERRUPT
	;-----------------------------------
	xor ax, ax
    mov es, ax ; point es to IVT base

	mov word[oldTimer], ax
	mov ax, [es:0x8*4]
	mov ax, [es:0x8*4+2]
	mov word[oldTimer+2], ax

	;HOOKING TIMER INTERRUPT
	;-----------------------------------
    cli                             ; disable interrupts
    mov word [es:8*4], timer        ; store offset at n*4
    mov [es:8*4+2], cs              ; store segment at n*4+2
    sti                             ; enable interrupts
	;-----------------------------------
	;_____________________________________________________
	;This is the play are of the GAME
	;-----------------------------------
	gamestart:
	call gen_foo
	mov word[food_pos], dx

	call clrscr
	call print_boundry
	call play_game

	exit:
		cmp byte[time_over], 1
		jnz next_check
		push t_over
		push 12
		push 1986
		call print_message
		jmp restore_everything

		next_check:
		;_____________________________________________________
		;This part contains checking of remaining lives and
		;playing the game again according to the remaining
		;number of lives
		;-----------------------------------
		dec word[cs:lives]
		cmp word[cs:lives], 0
		jz just_exit
		;Restoring TIMER and SCORE
		mov word[cs:seconds_remaining], 60
		mov word[cs:minutes_remaining], 3
		mov word[cs:score], 0

		mov word[delay], 40000
		mov word[tickcount], 0

		;Restoring the position and Length of SNAKE
		mov cx, 50
		mov bx, 0
		make_zero:
			mov word[snake_pos+bx], 0
			mov word[tempsnake_pos+bx], 0
			mov word[last_snake_state+bx], 0
			add bx, 2
			loop make_zero

		mov word[snake_pos+0], 1020
		mov word[snake_pos+2], 1022
		mov word[snake_pos+4], 1024
		mov word[snake_pos+6], 1026
		mov word[snake_pos+8], 1028

		mov word[tempsnake_pos+0], 1020
		mov word[tempsnake_pos+2], 1022
		mov word[tempsnake_pos+4], 1024
		mov word[tempsnake_pos+6], 1026
		mov word[tempsnake_pos+8], 1028

		mov word[snakelen], 5

		;Setting the the flags which 
		;make the SNAKE move in 
		;different directions

		mov word[up], 1
		mov word[down], 1
		mov word[forw], 1
		mov word[rev], 0

		mov word[uflag], 0
		mov word[dflag], 0
		mov word[fflag], 0
		mov word[rflag], 0
		mov word[exflag], 0

		;Restarting the game
		call gen_foo
		mov word[food_pos], dx

		call clrscr
		call print_boundry
		call play_game
		;-----------------------------------
		;_____________________________________________________

		just_exit:
		push game_over
		push 12
		push 1986
		call print_message

		restore_everything:
		xor ax, ax
		mov es, ax		;pointing es to IVT
		;RESTORING KEYBOARD
		;-----------------------------------
		cli
		mov ax, [oldisr]
		mov [es:0x9*4], ax
		mov ax, [oldisr+2]
		mov [es:0x9*4+2], ax
		sti
		;-----------------------------------

		;RESTORING TIMER
		;-----------------------------------
		cli
		mov ax, [oldTimer]
		mov [es:0x8*4], ax
		mov ax, [oldTimer+2]
		mov [es:0x8*4+2], ax
		sti
		;-----------------------------------

		;HAPPY ENDING (Giving control to DOS)
		;-----------------------------------
		mov ax,0x4c00
		int 21h