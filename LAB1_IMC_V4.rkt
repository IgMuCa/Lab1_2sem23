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

(define fl-op                  ; Dado un Flujo y una opcion, se obtienen CB y FW linkeados a partir de la lista de flujos
  (lambda (lst-fl fl-id op-id)
    (if (null? lst-fl) null
     (if (equal? fl-id (car (car lst-fl)))
         (op (car (cdr (cdr (car lst-fl)))) op-id)
     (fl-op (cdr lst-fl) fl-id op-id)))))

(define ch-fl-op               ; Dado un chatbot flujo y una opcion, se obtienen CB y FW linkeados a partir de la lista de chatbots
  (lambda (lst-cb ch-id fl-id op-id)
    (if (null? lst-cb) null
     (if (equal? ch-id (car (car lst-cb)))
         (fl-op (car(reverse(car lst-cb))) fl-id op-id)
     (ch-fl-op (cdr lst-cb) ch-id fl-id op-id)))))

;CONSTRUCTORES--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;OPCION (#2)
(define option
  (lambda (code menssage ChatbotCodeLink InitialFlowCodeLink . keyword)
    (list code menssage ChatbotCodeLink InitialFlowCodeLink keyword)))

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
      [ (member (car chatbot) (primeros-elem (get-system-chatbots system))) system ]
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
(define fnOp
  (lambda (op listOp)                            ;Lista de opciones del flujo de interes
    (if (null? listOp) null
        (if (or (equal? op (car (car listOp))) (member op (car (reverse listOp)))) (car (car listOp))
                 (fnOp (op cdr(listOp))))
         )))
    
(define system-talk-rec
  (lambda(system op)
    (if (equal? "" (get-system-CurrentUser system)) system
        (if (equal? 0  (interactions-system (get-system-ChatHistory system) (get-system-CurrentUser system)))        ;Se deben registrar en el historial       
            (list (get-system-name system)                                                                      ;La lista Historial se compone como lista de listas donde cada elemento sea del tipo (Usuario, fecha , (chatbot, flujo, opcion))
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
                                 (cons (cadr (reverse(last-interaction (get-system-CurrentUser system) (get-system-ChatHistory system))) )
                                       (cons (car (reverse (last-interaction (get-system-CurrentUser system) (get-system-ChatHistory system))))
                                             (cons op
                                                   (ch-fl-op
                                                          (get-system-chatbots system)
                                                          (cadr (reverse (last-interaction (get-system-CurrentUser system)(get-system-ChatHistory system))))
                                                          (car  (reverse (last-interaction (get-system-CurrentUser system)(get-system-ChatHistory system))))
                                                          op )))))
                                 (get-system-ChatHistory system))
                  (get-system-chatbots system)) 
                  ))))
;---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;INTERACTUAR CON UN CHATBOT (#13)
;Cuenta las veces que el usuario "user" ha interactuado con el sistema
(define interactions-system-sr   
  (lambda (lstCH user)
     (length(filter (lambda(x) (equal? user (car x))) lstCH))))

;Reporta la ultima interaccion que un usuario tuvo con el sistema
(define last-interaction-sr             
   (lambda (user lstCH)
     car(filter (lambda(x) (equal? user (car x))) (reverse lstCH))))
                 
;Esta función asigna un número si opcion es es un String
;(define fnOp-sr
 ; (lambda (op lstOp)                            ;Lista de opciones del flujo de interes
  ;    (filter (lambda(x) if (or  
    
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
                                 (cons (cadr (reverse(last-interaction (get-system-CurrentUser system) (get-system-ChatHistory system))) )
                                       (cons (car (reverse (last-interaction (get-system-CurrentUser system) (get-system-ChatHistory system))))
                                             (cons op
                                                   (ch-fl-op
                                                          (get-system-chatbots system)
                                                          (cadr (reverse (last-interaction (get-system-CurrentUser system)(get-system-ChatHistory system))))
                                                          (car  (reverse (last-interaction (get-system-CurrentUser system)(get-system-ChatHistory system))))
                                                          op )))))
                                 (get-system-ChatHistory system))
                  (get-system-chatbots system)) 
                  ))))

;SISTESIS DE LAS INTERACCIONE(#14)    
;Se obtiene el Chatbot de interes 
(define ObCb                                   
  (lambda (cb-id lstCb)
     (filter (lambda (x) (equal? cb-id (car x))) lstCb)))

;Se obtiene el Flujo de interes 
(define ObFw                                  
  (lambda (fw-id lstFw)
   (filter (lambda (x) (equal? fw-id (car x))) lstFw)))

;Se obtienen las opciones disponibles
(define ObOpciones                                  
  (lambda (lstOp)
    (map (lambda (x) (cadr x)) lstOp)))
 
;Se crea la funcion auxiliar que permita mantener como entradas de la función principal sólo, el system y user
(define system-synthesis
  (lambda (system user)
    (filter (lambda (x) (equal? user (cadr x)))
            (reverse(map(lambda (x) (list (cadr x)
                                          (car x)
                                          (cadr x)
                                          (caddr(caddr x))
                                          (cadr(car(ObCb (cadr(reverse(caddr x))) (get-system-chatbots system))))
                                          (cadr(car(ObFw (car(reverse(caddr x))) (car(reverse (car(ObCb (cadr(reverse(caddr x))) (get-system-chatbots s13))))) )))
                                          (ObOpciones (car(reverse(car(ObFw (car(reverse(caddr x))) (car(reverse (car(ObCb (cadr(reverse(caddr x))) (get-system-chatbots s13))))))))))                                        
                                          ))                       
                        (cadr (reverse s12)) )))
      
    ))


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


(define s11 (system-talk-rec s10 1))       s11
(define s12 (system-talk-rec s11 1))       s12
(define s13 (system-talk-rec s12 2))     s13
(define s14 (system-synthesis s13 "user2"))   s14
;(display (system-synthesis s13 "user2"))

