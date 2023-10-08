#lang racket
;OTROS--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;-----Asociados a listas----------------------------------------------------------------------------------------------------------------------------------------------------------------
;Añade elemento al final de una lista
;Dom: Elemento x Lista (Lista de elementos)
;Rec: Lista
(define addElemento
  (lambda (elemento lista)
  (reverse (cons elemento (reverse lista)))))

;Añade elemento al final de una lista utilizando recursión tail
;Dom: Elemento x Lista (Lista de elementos)
;Rec: Lista
(define addElemRecursion
  (lambda (elemento lista)
    (define addElemRec
      (lambda (elemento lista listAcc)
        (if (null? lista)
            (reverse (cons elemento listAcc))
            (addElemRec  elemento (cdr lista) (cons (car lista) listAcc) ))))
    (addElemRec elemento lista '())))

;Eliminar elementos repetidos en una lista
;Dom: Lista
;Rec: Lista
(define (eliminar-repetidos lista)
  (define (eliminar-auxiliar lista newlista)
    (cond
      [(null? lista) newlista] ; Cuando la lista original está vacía, retornamos la nueva lista
      [(member (car lista) newlista) ; Si el primer elemento ya está en la nueva lista, lo omitimos
       (eliminar-auxiliar (cdr lista) newlista)]
      [else ; Si el primer elemento no está en la nueva lista, lo agregamos
       (eliminar-auxiliar (cdr lista) (cons (car lista) newlista))] ))
  (reverse(eliminar-auxiliar lista '()) ))

;Obtiene la lista de los primeros elementos de una lista de listas
;Dom: Lista (Lista de listas)
;Rec: Lista
(define primeros-elem
  (lambda (lista)
    (map (lambda (lst) (car lst)) lista)))

;Obtiene la lista de los primeros elementos de una lista de listas
;Dom: Lista x Numero (Integer)
;Rec: Lista de numeros
(define op                     ; Dada una opcion, se obtienen CB y FW linkeado a partir de la lista de opciones
  (lambda (lst-op op-id)
    (if (null? lst-op) null
     (if (equal? op-id (car (car lst-op)))
         (list (car(cdr (cdr (car lst-op)))) (car(cdr (cdr (cdr (car lst-op)))))) 
     (op (cdr lst-op) op-id)))))

;Dom: Lista x Numero(Integer)x Numero(Integer)
;Rec: Lista 
(define fl-op                  ; Dado un Flujo y una opcion, se obtienen CB y FW linkeados a partir de la lista de flujos
  (lambda (lst-fl fl-id op-id)
    (if (null? lst-fl) null
     (if (equal? fl-id (car (car lst-fl)))
         (op (car (cdr (cdr (car lst-fl)))) op-id)
     (fl-op (cdr lst-fl) fl-id op-id)))))

;Dom: Lista x Numero(Integer)x Numero(Integer)
;Rec: Lista 
(define ch-fl-op               ; Dado un chatbot flujo y una opcion, se obtienen CB y FW linkeados a partir de la lista de chatbots
  (lambda (lst-cb ch-id fl-id op-id)
    (if (null? lst-cb) null
     (if (equal? ch-id (car (car lst-cb)))
         (fl-op (car(reverse(car lst-cb))) fl-id op-id)
     (ch-fl-op (cdr lst-cb) ch-id fl-id op-id)))))

;CONSTRUCTORES--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;OPCION (#2)
;Dom 
(define option
  (lambda (code message ChatbotCodeLink InitialFlowCodeLink . keyword)
    (list code message ChatbotCodeLink InitialFlowCodeLink keyword)))

;FLUJO (#3)
(define flow
  (lambda (code name . opciones)
    (list code name (eliminar-repetidos opciones))))

;CHATBOT (#5)
(define chatbot
  (lambda (code name welcomeMessage startFlowId . flows)
    (list code name welcomeMessage startFlowId (eliminar-repetidos flows))))

;SISTEMA de chatbots (#7)
(define system
  (lambda (name InitialChatbotCodeLink . chatbots)
    (list name InitialChatbotCodeLink '() "" '() (eliminar-repetidos chatbots))))

;GET------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define get-flow-id (lambda (flow) (car flow))) 
(define get-flow-name (lambda (flow) (car (cdr flow)))) 
(define get-flow-opciones (lambda (flow) (car (cdr (cdr flow))))) 

(define get-chatbot-id (lambda (chatbot) (car chatbot))) 
(define get-chatbot-name (lambda (chatbot) (car (cdr chatbot))))
(define get-chatbot-welcomeMessage (lambda (chatbot) (car (cdr (cdr chatbot)))))
(define get-chatbot-startFlowId (lambda (chatbot) (car (cdr (cdr (cdr chatbot))))))
(define get-chatbot-flows (lambda (chatbot) (car (cdr (cdr (cdr (cdr chatbot)))))))

(define get-system-name (lambda (system) (car system))) 
(define get-system-InitialChatbot (lambda (system) (car (cdr system))))
(define get-system-Users (lambda (system) (car (cdr (cdr system)))))
(define get-system-CurrentUser (lambda (system) (car (cdr (cdr (cdr system))))))
(define get-system-ChatHistory (lambda (system) (car (cdr (cdr (cdr (cdr system)))))))
(define get-system-chatbots (lambda (system) (car (cdr (cdr (cdr (cdr (cdr system))))))))


;SET------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;AGREGA UNA OPCION A UN FLUJO (#4)
(define flow-add-option
  (lambda(flow option)
    (cond
      [ (null? (get-flow-opciones flow)) (list (get-flow-id flow) (get-flow-name flow) (addElemento option (get-flow-opciones flow) )) ] ;; Se construye otro flow a partir del flow inicial
      [ (member (car option) (primeros-elem (get-flow-opciones flow))) flow ]
      [ else (list (get-flow-id flow) (get-flow-name flow) (addElemento option (get-flow-opciones flow) ))]
      ))) ;; Se construye otro flow a partir del flow inicial

;AGREGA UN FLUJO A UN CHATBOT (#6)
(define chatbot-add-flow
  (lambda(chatbot flow)
 (cond
      [(null? (get-chatbot-flows chatbot)) (list
                                             (get-chatbot-id chatbot)
                                             (get-chatbot-name chatbot)
                                             (get-chatbot-welcomeMessage chatbot)
                                             (get-chatbot-startFlowId chatbot)
                                             (addElemento flow (get-chatbot-flows chatbot) )) ] ;; Se construye otro CB a partir del CB inicial
      [(member (car flow) (primeros-elem (get-chatbot-flows chatbot))) chatbot ]
      [ else (list (get-chatbot-id chatbot)
                   (get-chatbot-name chatbot)
                   (get-chatbot-welcomeMessage chatbot)
                   (get-chatbot-startFlowId chatbot)
                   (addElemento flow (get-chatbot-flows chatbot)))]
      ))) 

;AGREGA UN CHATBOT A UN SISTEMA (#8)
(define system-add-chatbot
  (lambda(system chatbot)
 (cond
      [ (null? (get-system-chatbots system)) (list (get-system-name system)
                                                   (get-system-InitialChatbot system)
                                                   (get-system-Users system)
                                                   (get-system-CurrentUser system)
                                                   (get-system-ChatHistory system)
                                                   (addElemento chatbot (get-system-chatbots system)))] ;; Se construye otro system a partir del system inicial
      [ (member (car chatbot) (primeros-elem (get-system-chatbots system))) system ]   ;La funcion (primeros-elem list)entrega los primedor elementos de una lista de listas
      [ else (list (get-system-name system)
                   (get-system-InitialChatbot system)
                   (get-system-Users system)
                   (get-system-CurrentUser system)
                   (get-system-ChatHistory system)
                   (addElemento chatbot (get-system-chatbots system)))]
      )))

;AGREGA UN USUARIO A UN SISTEMA (#9)
(define system-add-user
  (lambda(system user)
    (if (member user (get-system-Users system)) system
        (list (get-system-name system) (get-system-InitialChatbot system) (addElemento user (get-system-Users system)) (get-system-CurrentUser system) (get-system-ChatHistory system) (get-system-chatbots system)))))

;INICIAR SESION CON UN USUARIO (#10)
(define system-login
  (lambda (system user)
    (if (and (member user (get-system-Users system)) (null? (get-system-CurrentUser system)))
        (list (get-system-name system) (get-system-InitialChatbot system) (get-system-Users system) user (get-system-ChatHistory system) (get-system-chatbots system))
        system)))

;CERRAR SESION (#11)
(define system-logout
  (lambda (system)
    (list (get-system-name system) (get-system-InitialChatbot system) (get-system-Users system) (list) (get-system-ChatHistory system) (get-system-chatbots system))))


;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;INTERACTUAR CON UN CHATBOT (#12)
;Cuenta las veces que el usuario "user" ha interactuado con el sistema
(define interactions-system   
  (lambda (lst user)
    (define interactions-system-aux
      (lambda (lst user acc)
        (if (null? lst) acc
            (if (equal? (car(car lst)) user)
                (interactions-system-aux (cdr lst) user (+ 1 acc))
                (interactions-system-aux (cdr lst) user acc)))))    
    (interactions-system-aux lst user 0)))

;Reporta la ultima interaccion que un usuario tuvo con el sistema
(define last-interaction              
   (lambda ( user lstHistory )
     (if (null? lstHistory) null
         (if (equal? user (car(car lstHistory)))
              (car (reverse (car lstHistory)))
              (last-interaction (cdr lstHistory) user)))))
                  
;Esta función asigna un número si opcion es es un String
(define lstFlujos  (lambda (CbNum lstCb)
                     (if (null? lstCb) null
                         (if (equal? CbNum (car (car lstCb)))
                             (car (reverse (car lstCb)))
                             (lstFlujos CbNum (cdr lstCb))))))

(define lstOpciones  (lambda (FwNum lstFw)
                     (if (null? lstFw) null
                         (if ( equal? FwNum (car (car lstFw)) )
                             (car (reverse (car lstFw)))
                             (lstFlujos FwNum (cdr lstFw))))))

(define OpFinal    (lambda (Op lstOp)                            ; (OpFinal Op  (lstOpciones FwNum (lstFlujos CbNum lstCb)))
                     (if (null? lstOp) null
                         (if  (or (equal? Op (car (car lstOp)))  (member Op (car(reverse(car lstOp)))) )
                               (car (car lstOp))
                               (OpFinal Op (cdr lstOp))))))
; Funcion propiamente tal
(define system-talk-rec
  (lambda(system op)
    (if (equal? "" (get-system-CurrentUser system)) system
        (if (equal? 0  (interactions-system (get-system-ChatHistory system) (get-system-CurrentUser system)))        ;Se deben registrar en el historial       
            (list (get-system-name system)                                                                           ;La lista Historial se compone como lista de listas donde cada elemento sea del tipo (Usuario, fecha , (chatbot, flujo, opcion))
                  (get-system-InitialChatbot system)
                  (get-system-Users system)
                  (get-system-CurrentUser system)
                  (cons(list
                        (get-system-CurrentUser system)
                        (current-seconds)
                        (list 0 1 0 0 1))                ;;  (Cb0,Fw0,OP,Cb1.Fw1)  / esto se trata de una condicion especial de inicio
                        (get-system-ChatHistory system)) ;;  El historychat se guardan 5 variables: chatbot, flujo, opcion_elegida, nuevo_chatbot, nuevo_flujo
                  (get-system-chatbots system))
            ;else
            (list (get-system-name system)
                  (get-system-InitialChatbot system)
                  (get-system-Users system) 
                  (get-system-CurrentUser system)
                  (cons (list
                        (get-system-CurrentUser system)
                        (current-seconds)
                                 (cons (cadr (reverse(last-interaction (get-system-CurrentUser system) (get-system-ChatHistory system)))) ;;CbNum
                                       (cons (car (reverse (last-interaction (get-system-CurrentUser system) (get-system-ChatHistory system)))) ;;FwNum
                                             (cons (OpFinal
                                                    op
                                                   (lstOpciones (car (reverse (last-interaction (get-system-CurrentUser system) (get-system-ChatHistory system))))
                                                                (lstFlujos (cadr (reverse(last-interaction (get-system-CurrentUser system) (get-system-ChatHistory system))))
                                                                                 (get-system-chatbots system))))
                                                   (ch-fl-op
                                                          (get-system-chatbots system)
                                                          (cadr (reverse (last-interaction (get-system-CurrentUser system)(get-system-ChatHistory system))));CbNum
                                                          (car  (reverse (last-interaction (get-system-CurrentUser system)(get-system-ChatHistory system)))) ;FwNum
                                                          (OpFinal
                                                           op
                                                          (lstOpciones (car (reverse (last-interaction (get-system-CurrentUser system) (get-system-ChatHistory system))))
                                                                  (lstFlujos (cadr (reverse(last-interaction (get-system-CurrentUser system) (get-system-ChatHistory system))))
                                                                                 (get-system-chatbots system))))
                                                          )))))
                                 (get-system-ChatHistory system))
                  (get-system-chatbots system)) 
                  ))))


;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;INTERACTUAR CON UN CHATBOT (SIN RECURSION) (#13)
;Cuenta las veces que el usuario "user" ha interactuado con el sistema
(define interactions-system-sr   
  (lambda (lstCH user)
     (length(filter (lambda(x) (equal? user (car x))) lstCH))))

;Reporta la ultima interaccion que un usuario tuvo con el sistema
(define last-interaction-sr             
   (lambda (user lstCH)
     (car(filter (lambda(x) (equal? user (car x)))  lstCH))))
                 
;Esta función asigna un número si opcion es es un String
(define OpFinal-sr (lambda (CbNum FwNum Op lstCb)                
                     (car (car (filter ( lambda (x)  (or (equal? Op (car x))   (member Op (car (reverse x)))))
                             (car (reverse (car(filter (lambda (x) (equal? FwNum (car x)))
                                                       (car(reverse (car (filter (lambda (x) (equal? CbNum (car x))) lstCb)))))))))))))
  
(define system-talk-rec-sr
  (lambda(system op)
    (if (equal? "" (get-system-CurrentUser system)) system
        (if (equal? 0  (interactions-system (get-system-ChatHistory system) (get-system-CurrentUser system)))        ;Se deben registrar en el historial       
            (list (get-system-name system)                                                                           ;La lista Historial se compone como lista de listas donde cada elemento sea del tipo (Usuario, fecha , (chatbot, flujo, opcion))
                  (get-system-InitialChatbot system)
                  (get-system-Users system)
                  (get-system-CurrentUser system)
                  (cons(list
                        (get-system-CurrentUser system)
                        (current-seconds)
                        (list 0 1 0 0 1))                ;;  (Cb0,Fw0,OP,Cb1.Fw1)  / esto se trata de una condicion especial de inicio
                        (get-system-ChatHistory system)) ;;  El historychat se guardan 5 variables: chatbot, flujo, opcion_elegida, nuevo_chatbot, nuevo_flujo
                  (get-system-chatbots system))
            ;else
            (list (get-system-name system)
                  (get-system-InitialChatbot system)
                  (get-system-Users system) 
                  (get-system-CurrentUser system)
                  (cons (list
                        (get-system-CurrentUser system)
                        (current-seconds)
                                 (cons (cadr (reverse(last-interaction (get-system-CurrentUser system) (get-system-ChatHistory system)))) ;;CbNum
                                       (cons (car (reverse (last-interaction (get-system-CurrentUser system) (get-system-ChatHistory system)))) ;;FwNum
                                             (cons (OpFinal-sr                                                                                              ;; Funcion que permite un string como opcion
                                                    (cadr (reverse(last-interaction (get-system-CurrentUser system) (get-system-ChatHistory system))))
                                                    (car (reverse (last-interaction (get-system-CurrentUser system) (get-system-ChatHistory system))))
                                                    op
                                                    (get-system-chatbots system)) 
                                                   (ch-fl-op
                                                          (get-system-chatbots system)   
                                                          (cadr (reverse (last-interaction (get-system-CurrentUser system)(get-system-ChatHistory system))))
                                                          (car  (reverse (last-interaction (get-system-CurrentUser system)(get-system-ChatHistory system))))
                                                          (OpFinal-sr                                                                                        ;; Funcion que permite un string como opcion
                                                                  (cadr (reverse(last-interaction (get-system-CurrentUser system) (get-system-ChatHistory system))))
                                                                  (car (reverse (last-interaction (get-system-CurrentUser system) (get-system-ChatHistory system))))
                                                                   op
                                                                  (get-system-chatbots system))
                                                          )))))
                                 (get-system-ChatHistory system))
                  (get-system-chatbots system)) 
                  ))))


;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;SISTESIS DE LAS INTERACCIONES(#14)    
;Se obtiene el Chatbot de interes (donde estan los flujos y las opciones que se buscan)
(define ObCb                                   
  (lambda (cb-id lstCb)
     (filter (lambda (x) (equal? cb-id (car x))) lstCb)))

;Se obtiene el Flujo de interes (donde estan las opciones que se buscan)
(define ObFw                                  
  (lambda (fw-id lstFw)
   (filter (lambda (x) (equal? fw-id (car x))) lstFw)))

;Se obtienen los nombres de las opciones disponibles. 
(define ObOpciones                                  
  (lambda (lstOp)
    (map (lambda (x) (cadr x)) lstOp)))
 
;Se crea la funcion auxiliar que permita mantener como entradas de la función principal sólo, el system y user
(define pre-synthesis
  (lambda (system user)
    (filter (lambda (x) (equal? user (cadr x)))
            (reverse(map(lambda (x) (list (cadr x)   ;Fecha
                                          (car x)    ;Usuario
                                          (cadr x)   ;Fecha
                                          (caddr(caddr x)) ;Opcion
                                          (cadr(car(ObCb (cadr(reverse(caddr x))) (get-system-chatbots system)))) ; nombre del chatbot
                                          (cadr(car(ObFw (car(reverse(caddr x))) (car(reverse (car(ObCb (cadr(reverse(caddr x))) (get-system-chatbots system))))) ))) ; nombre del flujo
                                          (ObOpciones (car(reverse (car(ObFw (car(reverse(caddr x))) (car(reverse (car(ObCb (cadr(reverse(caddr x))) (get-system-chatbots system))))))))))                                        
                                          ))                       
                      (cadr (reverse system)) )))  
    ))

;Funcion que para una lista cualquiera, despliega linea por linea los elementos de la misma
(define printlines (lambda (lista)
  (if (null? lista) ; Verificar si la lista está vacía
      (display " ")          ; Si está vacía, retornar una lista vacía
      (begin
        (display (car lista)) ; Imprimir el primer elemento de la lista
        (newline)             ; Imprimir una nueva línea
        (printlines (cdr lista))))))


;Funcion que despliega las interacciones de acuerdo a lo establecido en el LAB 1, usando printlines para el despliege de las opciones
(define printInterations (lambda (lista)
                (map (lambda (x)
                       (display (car x)) (display "-") (display (cadr x)) (display ":") (display (cadddr x))
                       (newline)
                       (display (car (cdr (cdr (cdr (cdr x)))))) (display " ") (display (car (cdr (cdr (cdr (cdr (cdr x)))))))
                       (newline)
                       (printlines  (car (reverse x))      )
                       (newline)
                       ) lista)))
                       

(define system-synthesis (lambda (system user)
                          (printInterations (pre-synthesis system user))))

                           
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;DEFINE SIMULADOR (#15)
; Cuenta las opciones asociadas a un cbNum (id chatbot) fwNum (id flujo)
(define contarOp (lambda ( system cbNum fwNum ) 
        (length(car (reverse (car (filter (lambda (x) (equal? fwNum  (car x))) 
                   (car (reverse (car(filter (lambda (x)  (equal? cbNum (car x)))  (get-system-chatbots system))))) )))))  ))

; Generador de numeros Random; genera numeros ramdom entre un maximo (sup) y minimo (inf)
(define fnRandom (lambda (inf sup)
  (+ inf (random (- sup inf)))))

; Generar un usuario dado un codigo numerico: Crea un usuario en el contexto de lo solicitado en lab
(define usuario_simulado (lambda (numero)  
          (string-append "user" (number->string numero)))) 

; Funcion que permite interactuar con el sistema dado usuario simulado y que no es parte de la lista de Users
(define system-talk-simulator  (lambda(system userSim)
      (if (member userSim (get-system-Users system)) system
        (if (equal? 0  (interactions-system (get-system-ChatHistory system) userSim ))        ;Se deben registrar en el historial       
            (list (get-system-name system)                                                                           ;La lista Historial se compone como lista de listas donde cada elemento sea del tipo (Usuario, fecha , (chatbot, flujo, opcion))
                  (get-system-InitialChatbot system)
                  (get-system-Users system)
                  (get-system-CurrentUser system)
                  (cons(list
                        userSim                               ;; Se incorpora el usuario simulado en el history chat del sistema
                        (current-seconds)
                        (list 0 1 0 0 1))                     ;;  (Cb0,Fw0,OP,Cb1.Fw1)  / esto se trata de una condicion especial de inicio
                        (get-system-ChatHistory system))      ;;  El historychat se guardan 5 variables: chatbot, flujo, opcion_elegida, nuevo_chatbot, nuevo_flujo
                  (get-system-chatbots system))
            ;else
            (list (get-system-name system)
                  (get-system-InitialChatbot system)
                  (get-system-Users system) 
                  (get-system-CurrentUser system)
                  (cons (list
                        userSim                               ;; Se incorpora el usuario simulado en el history chat del sistema
                        (current-seconds)
                                 (cons (cadr (reverse(last-interaction userSim (get-system-ChatHistory system))) ) ;;CbNum
                                       (cons (car (reverse (last-interaction userSim (get-system-ChatHistory system)))) ;;FwNum
                                             (cons (fnRandom  1 (contarOp system (cadr (reverse(last-interaction userSim (get-system-ChatHistory system))))    ;;Opcion simulada
                                                                                 (car (reverse (last-interaction userSim (get-system-ChatHistory system))))
                                                                                 ))
                                                   (ch-fl-op
                                                          (get-system-chatbots system)
                                                          (cadr (reverse (last-interaction userSim (get-system-ChatHistory system))))
                                                          (car  (reverse (last-interaction userSim (get-system-ChatHistory system))))
                                                          (fnRandom  1 (contarOp system (cadr (reverse(last-interaction userSim (get-system-ChatHistory system))))    ;;Opcion simulada
                                                                                 (car (reverse (last-interaction userSim (get-system-ChatHistory system))))
                                                                                 ))
                                                          )))))
                                 (get-system-ChatHistory system))
                  (get-system-chatbots system)) 
                  ))))

; Funcion que permite simular la conversación de un Chatbot (pero en una estructura recursiva que requiere un acumulador por lo que es la base para la función definitiva
; de acuerdo a lo requerido por el LAB
(define system-simulate-aux (lambda (system Acc Num_ite seed)                             
                              (if (< Acc Num_ite)
                                 (begin
                                     (system-talk-simulator system (usuario_simulado seed))                                     
                                     (system-simulate-aux (system-talk-simulator system (usuario_simulado seed)) (+ 1 Acc) Num_ite seed)
                                     (system-synthesis system (usuario_simulado seed)))
                                     (system-talk-simulator (system-talk-simulator system (usuario_simulado seed)) (usuario_simulado seed)) )))
                              

; Simulador propiamente tal
(define system-simulate (lambda (system Num_ite seed)
                  (system-simulate-aux system 0 Num_ite seed)))         


;SCRIPTS DE PRUEBA ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
; CHATBOT 0
(define op1 (option  1 "1) Viajar" 1 1 "viajar" "turistear" "conocer")) op1
(define op2 (option  2 "2) Estudiar" 2 1 "estudiar" "aprender" "perfeccionarme")) op2
(define f10 (flow 1 "Flujo Principal Chatbot 1\nBienvenido\n¿Qué te gustaría hacer?" op1 op2 op2 op2 op2 op1)) f10
(define f11 (flow-add-option f10 op1))  f11          
(define cb0 (chatbot 0 "Inicial" "Bienvenido\n¿Qué te gustaría hacer?" 1 f10 f10 f10 f10)) cb0

; CHATBOT 1
(define op3 (option 1 "1) New York, USA" 1 2 "USA" "Estados Unidos" "New York")) op3
(define op4 (option 2 "2) París, Francia" 1 1 "Paris" "Eiffel")) op4
(define op5 (option 3 "3) Torres del Paine, Chile" 1 1 "Chile" "Torres" "Paine" "Torres Paine" "Torres del Paine")) op5
(define op6 (option 4 "4) Volver" 0 1 "Regresar" "Salir" "Volver")) op6
(define op7 (option 1 "1) Central Park" 1 2 "Central" "Park" "Central Park")) op7
(define op8 (option 2 "2) Museos" 1 2 "Museo")) op8
(define op9 (option 3 "3) Ningún otro atractivo" 1 3 "Museo")) op9
(define op10 (option 4 "4) Cambiar destino" 1 1 "Cambiar" "Volver" "Salir")) op10
(define op11 (option 1 "1) Solo" 1 3 "Solo")) op11
(define op12 (option 2 "2) En pareja" 1 3 "Pareja")) op12
(define op13 (option 3 "3) En familia" 1 3 "Familia")) op13
(define op14 (option 4 "4) Agregar más atractivos" 1 2 "Volver" "Atractivos")) op14
(define op15 (option 5 "5) En realidad quiero otro destino" 1 1 "Cambiar destino")) op15
(define f20 (flow 1 "Flujo 1 Chatbot1\n¿Dónde te Gustaría ir?" op3 op4 op5 op6)) f20
(define f21 (flow 2 "Flujo 2 Chatbot1\n¿Qué atractivos te gustaría visitar?" op7 op8 op9 op10)) f21
(define f22 (flow 3 "Flujo 3 Chatbot1\n¿Vas solo o acompañado?" op11 op12 op13 op14 op15)) f22
(define cb1 (chatbot 1 "Agencia Viajes"  "Bienvenido\n¿Dónde quieres viajar?" 1 f20 f21 f22)) cb1

; CHATBOT 2
(define op16 (option 1 "1) Carrera Técnica" 2 1 "Técnica")) op16
(define op17 (option 2 "2) Postgrado" 2 1 "Doctorado" "Magister" "Postgrado")) op17
(define op18 (option 3 "3) Volver" 0 1 "Volver" "Salir" "Regresar")) op18
(define f30 (flow 1 "Flujo 1 Chatbot2\n¿Qué te gustaría estudiar?" op16 op17 op18)) f30
(define cb2 (chatbot 2 "Orientador Académico"  "Bienvenido\n¿Qué te gustaría estudiar?" 1 f30)) cb2

;SISTEMA
(define s0 (system "Chatbots Paradigmas" 0 cb0 cb0 cb0 cb1 cb2)) s0
(define s1 (system-add-chatbot s0 cb0))  s1
(define s2 (system-add-user s0 "user1")) s2
(define s3 (system-add-user s2 "user2")) s3
(define s4 (system-add-user s3 "user2")) s4
(define s5 (system-add-user s4 "user3")) s5
(define s6 (system-login s5 "user8"))    s6
(define s7 (system-login s6 "user1"))    s7
(define s8 (system-login s7 "user2"))    s8
(define s9 (system-logout s8))           s9
(define s10 (system-login s9 "user2"))   s10
(define s11 (system-talk-rec s10 "hola"))      s11
(define s12 (system-talk-rec s11 1))           s12
(define s13 (system-talk-rec s12 1))           s13
(define s14 (system-talk-rec s13 "Museo"))     s14
(define s15 (system-talk-rec s14 1))           s15
(define s16 (system-talk-rec s15 3))           s16
(define s17 (system-talk-rec s16 5))           s17

(display (system-synthesis s17 "user2"))




