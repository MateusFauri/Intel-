;-------------------------------------------------------------------------------------
;               TRABALHO INTEL
;       Autor:          Mateus Emer Fauri
;-------------------------------------------------------------------------------------

        .MODEL small                                            ; segmento de codigo e de dados NEAR

        .STACK 1024                                             ; pilha de 1k bytes

CR		equ		0dh
LF		equ		0ah

        .DATA                                                   ; inicio do segmento de dados

fileName        db		256 dup (?)                     ; nome do arquivo
entrada         db              256 dup (?)                     ; toda entrada na linha de comando
codigo          db              256 dup (?)                     ; string com o codigo da linha de comando
codigoHexa      db              256 dup (?)                     ; string com o codigo calculado transformado em Hexadecimal
FileHandle      dw		0		                ; "ponteiro" da leitura do arquivo
tamanhoEntrada  dw              0                               ; tamanho da entrada da linha de comando
flagG           db              0                               ; flag para -g
flagV           db              0                               ; flag para -v
flagAL          db              0                               ; flag usada na conversão hexadecimal
contador        db              0                               ; uma variavel contador
sw_n	        dw	        0                       
sw_f	        db	        0                       
sw_m	        dw	        0                       
buffer	        dw		0                               ; buffer da leitura do arquivo
tamanhoBArquivo dw              0                               ; numero de bytes do arquivo
x               dw              0                               ;  x     16 bits
                dw              0                               ;  x + 2 16 bits
                dw              0                               ;  x + 4 16 bits
                dw              0                               ;  x + 6 16 bits

; Mensagens que irão aparecer na tela caso houver algum erro
erroOpen	db	        "Erro na abertura do arquivo.", CR, LF, 0
erroRead	db	        "Erro na leitura do arquivo.", CR, LF, 0
CRLF		db	        CR, LF, 0

; Mensagens para Debug do codigo
;------------Debug------------------
MSGdiferentes	db	        "Diferentes.", CR, LF, 0
MSGiguais  	db	        "Iguais.", CR, LF, 0
string          db              100 dup (?)
;---------------------------
        .CODE                                                   ; inicio do segmento de codigo

        .STARTUP
        call    informacaoConsole                               ; inicializa string entrada

        call    processaLC                                      ; preprocessa a string entrada 
        
        call    tratamentoArquivo                               ; abre o arquivo e faz sua leitura
        cmp     al, 1                                           ; se retornar 1 em al, algo deu errado na abertura do arquivo
        je      end_program

verificarFlags:                                                 ; verificação das flagas V e G
        cmp     flagG, 1
        je      calcularCodigo                                  ; se G então calcula o codigo do arquivo

        cmp     flagV, 1
        JE      verificarCodigo                                 ; se V então compara os codigos

        jmp     end_program

calcularCodigo:
        call    converterHexaBit

;----------- Debug ---------------                              ; coloca o codigoHexa na tela
        lea     bx, codigoHexa                          
        call    printf

        lea     bx, CRLF
        call    printf
;---------------------------------
        mov     flagG,0                                         ; seta a flag G para zero
        jmp     verificarFlags                                  ; verifica se tem outra flag ligada

verificarCodigo:

        call    converterHexaBit
        cld                                                     ; limpa a flag de direção
        lea     di, codigo                                      ; coloca a string do codigo da linha de comando em di
        lea     si, codigoHexa                                  ; e coloca o codigo calculado em Hexadecimal em si

loopComparacao:
        cmpsb                                                   ; compara di[0] com si[0]
        jne     diferentes                                      ; se forem diferentes, pula para diferentes
        cmp     byte ptr [si], 0                                ; se não, compara se si acabou        
        JE      verificarFimOutro                               ; se não acabou compara se di acabou
        cmp     byte ptr [di], 0                                ; e compara se di acabou
        je      diferentes                                      ; se si não acabou e di acabou, são diferentes
        jmp     loopComparacao                                  ; se não, nenhum dos dois acabou, continua com cmpsb 

verificarFimOutro:                                      
        cmp     byte ptr [di],0                                 ; compara se di acabou (ja que si acabou)
        je      iguais                                          ; se si acabou e di acabou são iguais
        jmp     diferentes                                      ; se di não acabou, são diferentes ja que si acabou

diferentes:     
        lea     bx, MSGdiferentes                               ; se são diferentes, coloca uma msg de diferentes na tela
        call    printf
        jmp     fimVerificarCodigos                     
iguais:
        lea     bx, MSGiguais                                   ; se são iguais, coloca uma msg de iguais na tela
        call    printf

fimVerificarCodigos:

;-------------DEBUG---------------------                        ; coloca os valores dos dois na tela
        lea     bx, codigo      
        call    printf

        lea     bx, CRLF
        call    printf

        lea     bx, codigoHexa
        call    printf
;---------------------------------------
        mov     flagV,0                                         ; seta a flag V para zero
        jmp     verificarFlags                                  ; e ve se tem outro flag ligado


end_program:                                          
        .EXIT   0

;
;----------------------------------------
;    Abre e fecha o arquivo salvo em fileName
;    Escreve o tamanho em bytes do arquivo em tamanhoBArquivo
;----------------------------------------

tratamentoArquivo       proc    near

        mov     tamanhoBArquivo, 0                              ; inicializa em 0 a quantidade de bytes do arquivo
	mov	al, 0
	lea	dx, fileName
	mov	ah, 3dh
	int	21h                                             ; abre o arquivo apontado por fileName
	jnc	abriuArquivo                                    ; se abriu sem problemas pula 

	lea	bx,erroOpen                                     ; se teve problema coloca uma msg de erro na tela
	call	printf
	mov	al,1                                            ; coloca 1 em al e sai da subrotina

	jmp	final_tratamento

abriuArquivo:                                                   ; se abriu o arquivo 
        mov	FileHandle,ax                                   ; coloca o "ponteiro" do arquivo em fileHandle
        mov     word ptr x     , 0                              ; inicializa os 16 primeiros bytes de x com zero 
        mov     word ptr x + 2 , 0                              ; inicializa os 16 segundos bytes de x com zero 
        mov     word ptr x + 4 , 0                              ; inicializa os 16 terceiros bytes de x com zero 
        mov     word ptr x + 6 , 0                              ; inicializa os 16 quartos bytes de x com zero 
        mov     contador, 0                                     ; inicia o contador com 0

voltar:
	mov	bx,FileHandle                                   ; coloca o "ponteiro" do arquivo em bx
	mov	ah,3fh                                           
	mov	cx,1                                            ; vai ler apenas 1 byte do arquivo por vez
	lea	dx,buffer                                       ; coloca o endereço do buffer no dx 
	int	21h
	jnc	leuChar		                                ; se não deu erro na leitura, ele continua

	lea	bx,erroRead                                     ; se deu erro na leitura coloca uma msg de erro
	call	printf
	mov	al,1                                            ; coloca 1 em al

	jmp	fechar                                          ; vai fechar o arquivo e sair da subrotina

leuChar:
	cmp	ax,0                                            ; verifica se terminou o arquivo
	jne	naoTerminouArquivo                              ; se não terminou, ele continuia

	mov	al,0                                            ; move 0 para al (acabou sem problema a leitura do arquivo)
	jmp	fechar                                          ; se terminou, ele pula para fechar o arquivo     

naoTerminouArquivo:
        inc     tamanhoBArquivo                                 ; incrementa em 1 a quantidade de bytes do arquivo
        mov     dx, buffer
        add     x, dx                                           ; adiciona os primeiros 16 bytes de x com o que foi lido do arquivo
        jnc     seguirLendo                                     ; se não tiver carry, continua lendo o arquivo
        adc     [x + 2], 0                                      ; se tiver carry, adiciona o carry com os proximos 16 bits de x
        jnc     seguirLendo                                     ; continua fazendo essa verificação ate completar 64 bits
        adc     [x + 4], 0
        jnc     seguirLendo
        adc     [x + 6], 0

seguirLendo:
        jmp     voltar                                          ; continua lendo ate acabar o arquivo

fechar:
        mov	bx, FileHandle                                  ; fecha o arquivo do fileHandle
	mov	ah,3eh
	int	21h     

final_tratamento:                                               ; sai da subrotina
        ret

tratamentoArquivo endp


;
;----------------------------------------
;   Calcula o tamanho da entrada
;   e chama o processamento da
;   linha de comando
;----------------------------------------

processaLC      proc    near

        mov     cx, 0                                           ; coloca zero no registrador contador
        lea     si, entrada                                     ; incializa um ponteiro para entrada em si

loopCalculoEntrada:
        CMP     BYTE PTR [SI], 0                                ; compara um byte de si com zero, para achar o final da string
        JE      finalCalculo                            
        INC     CX                                              ; incremente cx e si e continua ate acabar
        INC     SI                  
        JMP     loopCalculoEntrada

finalCalculo:
        mov     tamanhoEntrada, cx
        call    processamentoEntrada                             ; chama a subrotina de processar a entrada
        ret

processaLC endp

;
;----------------------------------------
;   Separa a string de Entrada em:
;   -a nomeArquivo ->  coloca o nomeArquivo em fileName
;   -g ->  liga a flag G
;   -v codigo ->  copia o codigo Hexadecimal informado para a string codigo liga a flag V
;----------------------------------------

processamentoEntrada    proc    near

        lea     di, entrada                                     ; inicializa um ponteiro para entrada em di
        mov     cx, tamanhoEntrada                              ; coloca o tamanho da entrada em cx
buscaTraco:
        mov     ah, 0                                           ; busca o caracter '-' na string de entrada
        mov     al, '-' 
        repne   scasb                                           ; repete ate achar ou acabar o cx (tamanho da entrada)
        je      verificarOpcao                                  ; se achou 1, vai verificar cam a opção que vem logo a seguir

        jmp     fim_processamento

verificarOpcao:                                                 ; compara o digito a seguir do - com A G V
        mov  al, [di]                                                   
        cmp  al, 'a'
        je   opcao_a                                            ; se for A vai para a parte opcao a
        cmp  al, 'g'
        je   opcao_g                                            ; se for G vai para a parte opcao g
        cmp  al, 'v'
        je   opcao_v                                            ; se for V vai para a parte opcao v

        jmp  fim_processamento                                  ; se não for nenhum deles sai da subrotina

opcao_a:
        mov     si, di                                          ; move o ponteiro da entrada para si
        add     si, 2                                           ; adiciona dois para pular o espaço
        lea     di, fileName                                    ; inicializa um ponteiro para fileName
        cld
loopFileName:
        CMP     BYTE PTR [si], 0                                ; compara se chegou ao final da string Entrada
        JE      finalString
        CMP     BYTE PTR [si], '-'                              ; compara se achou outro '-'
        je      finalStringContinua                             
        movsb                                                   ; move um byte da si para di, incrementando os dois
        JMP     loopFileName                                    ; continua ate cais em um dos casos acima
finalStringContinua:                                            ; se achou outro '-'
        dec     di                                              ; decrementa um de di
        mov	byte ptr [di],0                                 ; "fecha a string" filename
        inc     si                                              ; incrementa 1 de si (para pegar a opção apos '-')
        mov     di, si                                          ; muda si para di novamente
        jmp     verificarOpcao                                  ; verifica que outra opção esta presente
finalString:                                                    ; não achou outro '-'
        mov	byte ptr [di],0                                 ; então só 'acaba' a string 
        jmp     fim_processamento                               ; e sai da subrotina

opcao_v:
        mov     flagV, 1                                        ; coloca 1 na flagV, para testar mais tarde
        mov     si, di                                          ; di estava apontando para entrada, agora si é que esta
        add     si, 2
        lea     di, codigo                                      ; e di agora aponta para string onde precisa escrever o codigo
        cld
loopcodigo:                             
        CMP     BYTE PTR [si], 0                                ; compara se chegou ao fim da string
        JE      finalCodigo
        CMP     BYTE PTR [si], '-'                              ; compara se chegou a outro comando
        je      finalStrigCodigo         
        movsb
                                                                ; aqui ate continua loop codigo faz todas as letras minusculas
	cmp	[di - 1],'a'                                    ; da string codigo serem maiusculas
	jb	continuaLoopCodigo
	cmp	[di - 1],'z'
	ja	continuaLoopCodigo
	sub	[di - 1],20h		
continuaLoopCodigo:
        JMP     loopcodigo                                      ; apos transformar todas em maiusculas, pula para outra leitura

finalStrigCodigo:
        dec     di                                              ; decremente para não pegar o espaço
        mov	byte ptr [di],0                                 ; coloca um 'fim' na string
        inc     si
        mov     di, si                                          ; coloca a string 'entrada' em di novamente 
        jmp     verificarOpcao                                  ; e verifica a opcao de novo, pois achou um '-'

finalCodigo:
        mov	byte ptr [di],0
        jmp     buscaTraco                                      ; chegou volta para verificar se tem outro comando

opcao_g:
        mov     flagG, 1                                        ; coloca 1 na flag G
        jmp     buscaTraco                                      ; chegou volta para verificar se tem outro comando

fim_processamento:
        ret

processamentoEntrada    endp



;---------------------------------------
;       Converte o numero na variavel X
;       em uma string hexadecimal 
;       codigoHexa
;---------------------------------------

converterHexaBit     proc       near
        lea     di, codigoHexa                                  ; inicializa um ponteiro para inicio do codigoHexa
        mov     cl, 4                                           ; vai percorrer 4 vezes (16 bits por vez) a variavel x
        mov     ax, x + 6                                       ; começando pelos 16 bits mais significativos
        cmp     ax, 0                                           ; ve se contem algum numero nesses 16 bits
        je      proximosBytes                                   ; se não tiver, pula para os proximos 16 bits
        cmp     ah, 0                                           ; se tiver, ve se tem algo em ah
        jne     resetarRotacao                                  ; se tiver, faz o shift 4 vezes (pegando 4 bits cada vez)
        mov     flagAL, 1                                       ; se não, ativa a flag AL (ira pegar só 8 bits do al ao invez de 16)
resetarRotacao:
        cmp     flagAL,1                                        ; ve se a flagAL esta ativa
        je      resetarRotacaoAL                                ; se estiver, vai para a preparação da rotação com 8 bits
        mov     dx, 0                                           ; se não, faz a preparação da rotação com 16 bits (rota 4 vezes ao invez de 2)
        mov     ch, 4
        jmp     rotacionar
resetarRotacaoal:
        mov     dx, 0                                           ; roda todo AH ate que AL fiquei em AH e AL fique zerado
        shl     ax,1
        shl     ax,1
        shl     ax,1
        shl     ax,1
        shl     ax,1
        shl     ax,1
        shl     ax,1
        shl     ax,1
        mov     ch,2
rotacionar:                                                     ; rota 4 bits do AH para dx
        shl     ax, 1                              
        rcl     dx, 1 
        shl     ax, 1                              
        rcl     dx, 1 
        shl     ax, 1                              
        rcl     dx, 1 
        shl     ax, 1                              
        rcl     dx, 1
        jmp	conversao                                       ; faz a conversão desses 4 bits para hexadecimal
conferirProximos:
        mov	dx, 0                                           ; limpa dx
        dec     ch                                              ; decrementa o contador de bits 
        cmp     ch, 0                                           ; ve se ainda tem 4 bits para serem lidos
        jne     rotacionar                                      ; se tiver, faz a rotação
        jmp	proximosBytes                                   ; se não tiver, vai para os proximos 16 bits

conversao:
        clc                                                     ; limpa o carry
        cmp     dl, 9                                           ; compara se dl é maior que 9 (se for vai ser transformado em letra)
        ja      transformarLetradl
        add     dl, '0'                                         ; se não for, transforma o numero no codigo ascii
        mov     [di], dl                                        ; e coloca na string
        inc     di                                              ; move o ponteiro da string
        jmp     conferirProximos                                ; volta para conferir se ainda tem mais bits a serem lidos

transformarLetradl:                                             ; se dl for maior que 9 vem para ca
        sub     dl, 10                                          ; diminui dl de 10, para fazer certo a conversão
        add     dl, 'A'                                         ; transforma dl para o codigo ascii dele
        mov     [di], dl                                        ; coloca ele na string
        inc     di                                              ; move o ponteiro da string
        jmp     conferirProximos                                ; volta para conferir se ainda tem mais bits a serem lidos

proximosBytes:
        dec     cl                                              ; decrementa cl (numero de 16 bits ja lidos)
        mov     flagAL,0                                        ; limpa a flag AL
        cmp     cl, 3                                           ; compara se é para x + 3
        je      proximo4byte                    
        cmp     cl, 2                                           ; compara se é para pegar x + 2
        je      proximo2byte
        cmp     cl, 1                                           ; compara se é para pegar x 
        je      proximobyte
        cmp     cl, 0                                           ; se cl 0, acaba a subrotina
        je      fimConverter

proximo4byte:
        mov     ax, x+4                                         ; move x+4 para ax, 
        cmp     ax, 0                                           ; compara se ax é zero
        je      proximosBytes                                   ; se for, vai para os proximos 16 bits
        cmp     ah, 0                                           ; se não for, compara se ah é igual a zero
        jne     resetarRotacao                                  ; se não for, vai para rotacionar 4 vezes
        mov     flagAL, 1                                       ; se for, liga a flag AL e vai rotacionar 2 vezes
        jmp     resetarRotacao
proximo2byte:
        mov     ax, x+2                                         ; move x+2 para ax, 
        cmp     ax, 0                                           ; compara se ax é zero
        je      proximosBytes                                   ; se for, vai para os proximos 16 bits
        cmp     ah, 0                                           ; se não for, compara se ah é igual a zero
        jne     resetarRotacao                                  ; se não for, vai para rotacionar 4 vezes
        mov     flagAL, 1                                       ; se for, liga a flag AL e vai rotacionar 2 vezes
        jmp     resetarRotacao
proximobyte:
        mov     ax, x                                           ; move x para ax, 
        cmp     ax, 0                                           ; compara se ax é zero
        je      proximosBytes                                   ; se for, vai para os proximos 16 bits
        cmp     ah, 0                                           ; se não for, compara se ah é igual a zero
        jne     resetarRotacao                                  ; se não for, vai para rotacionar 4 vezes
        mov     flagAL, 1                                       ; se for, liga a flag AL e vai rotacionar 2 vezes
        jmp     resetarRotacao

fimConverter:
        mov    byte ptr [di], 0                                 ; coloca um fim na string
        ret                                                     ; sai da subrotina
converterHexaBit endp

;----------------------------------------
;       Funções pegadas das aulas
;----------------------------------------

;
;----------------------------------------
;      Pega a linha de comando digitada
;      e coloca na string entrada
;----------------------------------------
informacaoConsole       proc    near
        push    ds                                              ; salva as informações de segmentos
        push    es
        mov     ax,ds                                           ; troca DS <-> ES, para poder usa o MOVSB
        mov     bx,es                  
        mov     ds,bx                  
        mov     es,ax
        mov     si,80h                                          ; obtém o tamanho do string e coloca em CX
        mov     ch,0
        mov     cl,[si]
        mov     si, 81h                                         ; inicializa o ponteiro de origem
        lea     di, entrada                                     ; inicializa o ponteiro de destino
        rep     movsb
        mov	byte ptr es:[di],0
        pop     es                                              ; retorna as informações dos registradores de segmentos
        pop     ds

        mov	ax,ds
        mov	es,ax

        ret
informacaoConsole       endp


;
;----------------------------------------
;       Coloca na tela a string
;       que esta tem seu inicio em bx
;----------------------------------------
printf    proc    near
       
	mov	dl,[bx]
	cmp	dl,0
	je	ps_1

	push    bx
	mov	ah,2
	int	21H
	pop	bx

	inc	bx
		
	jmp	printf
		
ps_1:
	ret

printf    endp


;
;--------------------------------------------------------------------
;Funcao: Converte um inteiro (n) para (string)
;		 sprintf(string, "%d", n)
;
;Associacao de variaveis com registradores e mem�ria
;	string	        -> bx
;	k		-> cx
;	m		-> sw_m dw
;	f		-> sw_f db
;	n		-> sw_n	dw
;--------------------------------------------------------------------

sprintf_w	proc	near
	mov		sw_n,ax
	mov		cx,5
	mov		sw_m,10000
	mov		sw_f,0
sw_do:
	mov		dx,0
	mov		ax,sw_n
	div		sw_m
	cmp		al,0
	jne		sw_store
	cmp		sw_f,0
	je		sw_continue
sw_store:
	add		al,'0'
	mov		[bx],al
	inc		bx
	
	mov		sw_f,1
sw_continue:
	mov		sw_n,dx
	mov		dx,0
	mov		ax,sw_m
	mov		bp,10
	div		bp
	mov		sw_m,ax
	dec		cx
	cmp		cx,0
	jnz		sw_do
	cmp		sw_f,0
	jnz		sw_continua2
	mov		[bx],'0'
	inc		bx
sw_continua2:
	mov		byte ptr[bx],0
	ret
sprintf_w	endp


;--------------------
;   Fim do Programa
;--------------------
        END


