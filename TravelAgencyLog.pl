% Definición de la base de datos con enfoque de grafo -----------------------------------------
% Los aeropuertos son los nodos y los vuelos son los arcos
% Aeropuertos: aeropuerto(NombreLargo, Codigo).
aeropuerto('San José, Costa Rica', sjo).
aeropuerto('Panamá', pty).
aeropuerto('Miami, EE.UU.', mia).
aeropuerto('Phoenix, Arizona', phx).
aeropuerto('Frankfurt, Alemania', fra).
aeropuerto('Ciudad de México', mex).
aeropuerto('Bogotá, Colombia', bog).
aeropuerto('Lima, Perú', lim).
aeropuerto('Dallas-Fort Worth, EE.UU.', dfw).
aeropuerto('Toronto, Canadá', yyz).
aeropuerto('Atlanta, EE.UU.', atl).
aeropuerto('Los Angeles, EE.UU.', lax).
aeropuerto('Madrid, España', mad).
aeropuerto('Londres, Reino Unido', lhr).
aeropuerto('París, Francia', cdg).

% Arcos: arco(Origen, Destino,vuelo(Numero, Aerolinea, Clase, Costo, Duracion, Tipo).
arco(sjo, pty, vuelo(cm404, avianca, negocios, 500, 1, comercial)).
arco(sjo, pty, vuelo(cm405, copa, negocios, 1500, 1, charter)).
arco(sjo, pty, vuelo(cm406, american, economica, 500, 1, comercial)).
arco(pty, phx, vuelo(ua105, united, economica, 900, 6, comercial)).
arco(pty, phx, vuelo(ua106, united, negocios, 1900, 6, charter)).
arco(pty, phx, vuelo(ua107, copa, economica, 900, 6, comercial)).
arco(sjo, mia, vuelo(ta321, avianca, economica, 450, 3, comercial)).
arco(mia, phx, vuelo(aa876, american, negocios, 1300, 5, comercial)).
arco(sjo, phx, vuelo(ch101, charter_air, economica, 1100, 7, charter)).
arco(sjo, pty, vuelo(ch202, charter_air, negocios, 1200, 1, charter)).
arco(phx, sjo, vuelo(ab976, american, negocios, 600, 8, comercial)).
arco(pty, phx, vuelo(ua777, united, negocios, 1500, 6, comercial)).
arco(sjo, fra, vuelo(lh450, lufthansa, negocios, 2000, 11, comercial)).
arco(fra, phx, vuelo(fra203, united, economica, 1300, 12, comercial)).
arco(sjo, mex, vuelo(am888, aeromexico, economica, 400, 2, comercial)).
arco(sjo, bog, vuelo(cp201, copa, economica, 350, 2, comercial)).
arco(sjo, bog, vuelo(av450, avianca, negocios, 550, 2, comercial)).
arco(sjo, lim, vuelo(la302, latam, economica, 480, 3, comercial)).
arco(sjo, lim, vuelo(la303, latam, negocios, 780, 3, comercial)).
arco(sjo, dfw, vuelo(aa201, american, economica, 650, 4, comercial)).
arco(sjo, yyz, vuelo(ac100, air_canada, negocios, 900, 5, comercial)).
arco(bog, mia, vuelo(av451, avianca, economica, 400, 3, comercial)).
arco(bog, mia, vuelo(av452, avianca, negocios, 800, 3, comercial)).
arco(lim, phx, vuelo(la305, latam, economica, 750, 7, comercial)).
arco(lim, phx, vuelo(la306, latam, negocios, 1400, 7, comercial)).
arco(dfw, phx, vuelo(aa202, american, economica, 300, 2, comercial)).
arco(dfw, phx, vuelo(aa203, american, negocios, 600, 2, comercial)).
arco(yyz, phx, vuelo(ac101, air_canada, economica, 450, 4, comercial)).
arco(mia, atl, vuelo(dl401, delta, economica, 200, 2, comercial)).
arco(mia, atl, vuelo(dl402, delta, negocios, 500, 2, comercial)).
arco(atl, phx, vuelo(dl403, delta, economica, 300, 3, comercial)).
arco(atl, phx, vuelo(dl404, delta, negocios, 700, 3, comercial)).
arco(mex, lax, vuelo(am889, aeromexico, economica, 350, 3, comercial)).
arco(mex, phx, vuelo(am890, aeromexico, economica, 320, 3, comercial)).
arco(mex, phx, vuelo(am891, aeromexico, negocios, 720, 3, comercial)).
arco(sjo, bog, vuelo(ch103, charter_air, negocios, 800, 2, charter)).
arco(bog, phx, vuelo(ch104, charter_air, negocios, 1500, 8, charter)).
arco(sjo, mex, vuelo(ch105, charter_air, economica, 750, 2, charter)).
arco(pty, lim, vuelo(pr100, priority_air, negocios, 900, 3, charter)).
arco(lim, fra, vuelo(pr101, priority_air, negocios, 2200, 12, charter)).
arco(fra, mad, vuelo(lh451, lufthansa, economica, 200, 2, comercial)).
arco(mad, mia, vuelo(lh452, lufthansa, economica, 800, 9, comercial)).
arco(lhr, phx, vuelo(ba100, british_airways, negocios, 1700, 10, comercial)).
arco(cdg, mia, vuelo(af200, air_france, economica, 900, 8, comercial)).
arco(sjo, cdg, vuelo(af201, air_france, negocios, 1800, 10, comercial)).

% Lista de aerolíneas disponibles
aerolinea(copa).
aerolinea(united).
aerolinea(avianca).
aerolinea(american).
aerolinea(charter_air).
aerolinea(lufthansa).
aerolinea(aeromexico).
aerolinea(latam).
aerolinea(delta).
aerolinea(air_canada).
aerolinea(british_airways).
aerolinea(air_france).
aerolinea(priority_air).

% Dinámicos
:- dynamic origen/1, destino/1, vuelo_charter/1, preferencia_clase/1, preferencia_aerolinea/1, presupuesto/1.

% Reglas para identificar BNF-------------------------------------------

% Frases válidas para indicar el origen
oracion_origen(origen(Ciudad)) -->
    saludo, opcionales, frase_ubicacion, ciudad(Ciudad), opcionales.
oracion_origen(origen(Ciudad)) -->
    frase_ubicacion, ciudad(Ciudad), opcionales.
oracion_origen(origen(Ciudad)) -->
    saludo, ciudad(Ciudad), opcionales.
oracion_origen(origen(Ciudad)) -->
    ciudad(Ciudad), opcionales.

oracion_destino(destino(Ciudad)) -->
    saludo, opcionales, verbo_ir, ciudad(Ciudad), opcionales.
oracion_destino(destino(Ciudad)) -->
    [mi, destino, es], ciudad(Ciudad), opcionales.
oracion_destino(destino(Ciudad)) -->
    verbo_ir, ciudad(Ciudad), opcionales.
oracion_destino(destino(Ciudad)) -->
    ciudad(Ciudad), opcionales.

% Reglas para palabras que se pueden ignorar si aparecen
% Palabras de saludo
saludo --> [hola].
saludo --> [hola, travelagencylog].
saludo --> [buenos, dias].
saludo --> [buenas, tardes].
saludo --> [buenas, noches].

frase_ubicacion --> [estoy, en].
frase_ubicacion --> [me, encuentro, en].
frase_ubicacion --> [ubicado, en].
frase_ubicacion --> [mi, ubicacion, es].
frase_ubicacion --> [me, hallo, en].

% Palabras que podrían formar parte de una ciudad (para no ignorarlas por error)
ciudad_palabra(P) :- member(P, [san, jose, costa, rica, panama, frankfurt, miami, arizona, mexico, bogota, lima, dallas, toronto, atlanta, los, angeles, madrid, londres, paris]).

ciudad(sjo) --> [san, jose].
ciudad(sjo) --> [san, jose, costa, rica].
ciudad(pty) --> [panama].
ciudad(phx) --> [arizona].
ciudad(mia) --> [miami].
ciudad(mex) --> [mexico].
ciudad(fra) --> [frankfurt].
ciudad(bog) --> [bogota].
ciudad(lim) --> [lima].
ciudad(dfw) --> [dallas].
ciudad(yyz) --> [toronto].
ciudad(atl) --> [atlanta].
ciudad(lax) --> [los, angeles].
ciudad(mad) --> [madrid].
ciudad(lhr) --> [londres].
ciudad(cdg) --> [paris].

verbo_ir --> [viajar, a]; [ir, a]; [voy, a]; [deseo, viajar, a]; [quiero, ir, a]; [me, gustaria, viajar, a].

vuelo_charter --> [charter]; [chárter].
oracion_charter(si) --> [deseo, un, vuelo], vuelo_charter.
oracion_charter(si) --> [me, gustaria, un], vuelo_charter.
oracion_charter(si) --> [si].
oracion_charter(no) --> [no, necesariamente].
oracion_charter(no) --> [no, necesariamente, deseo, el ,vuelo ,mas ,barato].
oracion_charter(no) --> [no].

clase(economica) --> [economica].
clase(negocios) --> [negocios].
oracion_clase(clase(Clase)) --> [en, clase, de], clase(Clase).
oracion_clase(clase(economica)) --> [economica].
oracion_clase(clase(negocios)) --> [negocios].

oracion_presupuesto(presupuesto(Cantidad)) --> [si], numero(Cantidad), [dolares].
oracion_presupuesto(presupuesto(no)) --> [no].
numero(N) --> [S], { atom_number(S, N) }.

opcionales --> [].
opcionales --> [Palabra], { \+ ciudad_palabra(Palabra) }, opcionales.

% Elimina todos los signos de puntuacion, convierte a minuscula los
% strings de entrada y crea tokens a partir de esta entrada
tokenize_input(Str, Tokens) :-
    downcase_atom(Str, Lower),
    split_string(Lower, " ", ".,!?", List),
    maplist(atom_string, Tokens, List).

% Interfaz de usuario---------------------------------------------------

start :-
    retractall(origen(_)),
    retractall(destino(_)),
    retractall(vuelo_charter(_)),
    retractall(preferencia_clase(_)),
    retractall(preferencia_aerolinea(_)),
    retractall(presupuesto(_)),
    writeln("TravelAgencyLog: Bienvenido a TravelAgencyLog la mejor lógica de llegar a su destino."),
    ask_origen.

ask_origen :-
    writeln("Por favor indíqueme cuál es el origen de su vuelo."),
    read_line_to_string(user_input, Input),
    tokenize_input(Input, Tokens),
    ( phrase(oracion_origen(origen(Ciudad)), Tokens) ->
        assertz(origen(Ciudad)),
        ask_destino
    ; writeln("Me podría repetir, no entendí."), ask_origen ).

ask_destino :-
    writeln("¿Cuál es su destino?"),
    read_line_to_string(user_input, Input),
    tokenize_input(Input, Tokens),
    ( phrase(oracion_destino(destino(Ciudad)), Tokens) ->
        assertz(destino(Ciudad)),
        ask_charter
    ; writeln("Me podría repetir, no entendí."), ask_destino ).

ask_charter :-
    writeln("¿Desea un vuelo Chárter?"),
    read_line_to_string(user_input, Input),
    tokenize_input(Input, Tokens),
    ( phrase(oracion_charter(R), Tokens) ->
        assertz(vuelo_charter(R))
    ; assertz(vuelo_charter(no)) ),
    ask_aerolinea.

ask_aerolinea :-
    writeln("¿Tiene alguna Aerolínea de preferencia?"),
    read_line_to_string(user_input, Input),
    tokenize_input(Input, Tokens),
    ( Tokens = [no] ->
        assertz(preferencia_aerolinea(no))
    ; member(copa, Tokens) ->
        assertz(preferencia_aerolinea(copa))
    ; member(united, Tokens) ->
        assertz(preferencia_aerolinea(united))
    ; member(avianca, Tokens) ->
        assertz(preferencia_aerolinea(avianca))
    ; member(american, Tokens) ->
        assertz(preferencia_aerolinea(american))
    ; member(charter_air, Tokens) ->
        assertz(preferencia_aerolinea(charter_air))
    ; member(lufthansa, Tokens) ->
        assertz(preferencia_aerolinea(lufthansa))
    ; member(aeromexico, Tokens) ->
        assertz(preferencia_aerolinea(aeromexico))
    ; member(latam, Tokens) ->
        assertz(preferencia_aerolinea(latam))
    ; member(delta, Tokens) ->
        assertz(preferencia_aerolinea(delta))
    ; member(air_canada, Tokens) ->
        assertz(preferencia_aerolinea(air_canada))
    ; member(british_airways, Tokens) ->
        assertz(preferencia_aerolinea(british_airways))
    ; member(air_france, Tokens) ->
        assertz(preferencia_aerolinea(air_france))
    ; member(priority_air, Tokens) ->
        assertz(preferencia_aerolinea(priority_air))
    ; assertz(preferencia_aerolinea(no))
    ),
    ask_clase.

ask_clase :-
    writeln("¿Tiene alguna clase preferencia (económica-negocios)?"),
    read_line_to_string(user_input, Input),
    tokenize_input(Input, Tokens),
    ( Tokens = [no] ->
        assertz(preferencia_clase(no))
    ; phrase(oracion_clase(clase(Clase)), Tokens) ->
        assertz(preferencia_clase(Clase))
    ; assertz(preferencia_clase(no)) ),
    ask_presupuesto.

ask_presupuesto :-
    writeln("¿Tiene algún presupuesto?"),
    read_line_to_string(user_input, Input),
    tokenize_input(Input, Tokens),
    ( phrase(oracion_presupuesto(presupuesto(Cantidad)), Tokens) ->
        assertz(presupuesto(Cantidad))
    ; assertz(presupuesto(no)) ),
    buscar_vuelo.

% Función para normalizar filtros
normalizar_filtros(TipoFiltro, AerolineaPref, ClasePref, Pres, PriorizarDuracion) :-
    % Procesamiento de tipo de vuelo
    (vuelo_charter(si) ->
        TipoFiltro = charter
    ; vuelo_charter(no) ->
        TipoFiltro = comercial
    ;
        TipoFiltro = _
    ),

    % Procesamiento de aerolínea
    (preferencia_aerolinea(Aero), Aero \= no ->
        AerolineaPref = Aero
    ;
        AerolineaPref = _
    ),

    % Procesamiento de clase
    (preferencia_clase(Clase), Clase \= no ->
        ClasePref = Clase
    ;
        ClasePref = _
    ),

    % Si el presupuesto es "no", activamos la priorización por duración
    (presupuesto(no) ->
        Pres = _,
        PriorizarDuracion = si
    ; presupuesto(P), atom(P) ->
        atom_number(P, Pres),
        PriorizarDuracion = no
    ; presupuesto(P), number(P) ->
        Pres = P,
        PriorizarDuracion = no
    ;
        Pres = _,
        PriorizarDuracion = no
    ).

% ALGORITMOS DE BÚSQUEDA EN GRAFO --------------------------------------

% Predicado para verificar si una arista cumple con los filtros
arista_filtrada(Origen, Destino, vuelo(Num, Aer, Cla, Cos, Dur, Tipo), TipoFiltro, AerFiltro, ClaFiltro, Pres) :-
    arco(Origen, Destino, vuelo(Num, Aer, Cla, Cos, Dur, Tipo)),
    % Verificar tipo de vuelo
    (var(TipoFiltro) ; Tipo = TipoFiltro),
    % Verificar aerolínea
    (var(AerFiltro) ; Aer = AerFiltro),
    % Verificar clase
    (var(ClaFiltro) ; Cla = ClaFiltro),
    % Verificar presupuesto
    (var(Pres) ; (number(Pres), Cos =< Pres)).

% Búsqueda de vuelos directos que cumplen los filtros
buscar_vuelos_directos(Origen, Destino, TipoFiltro, AerFiltro, ClaFiltro, Pres, Vuelos) :-
    findall([Origen, [Destino, Aer, Num, Dur, Cos]],
            arista_filtrada(Origen, Destino, vuelo(Num, Aer, _, Cos, Dur, _), TipoFiltro, AerFiltro, ClaFiltro, Pres),
            Vuelos).

% Búsqueda de vuelos con una escala que cumplen los filtros
buscar_vuelos_con_escala(Origen, Destino, TipoFiltro, AerFiltro, ClaFiltro, Pres, Vuelos) :-
    findall([Origen, [Escala, Aer1, Num1, Dur1, Cos1], [Destino, Aer2, Num2, Dur2, Cos2]],
            (arista_filtrada(Origen, Escala, vuelo(Num1, Aer1, Cla1, Cos1, Dur1, _), TipoFiltro, AerFiltro, ClaFiltro, _),
             arista_filtrada(Escala, Destino, vuelo(Num2, Aer2, Cla2, Cos2, Dur2, _), TipoFiltro, AerFiltro, ClaFiltro, _),
             Escala \= Destino, Escala \= Origen,  % Asegurarnos que la escala no sea ni origen ni destino
             % Verificación conjunta del presupuesto
             CostoTotal is Cos1 + Cos2,
             (var(Pres) ; (number(Pres), CostoTotal =< Pres)),
             % Para flexibilidad, permitimos que al menos uno de los vuelos cumpla con preferencias específicas
             ((var(AerFiltro) ; Aer1 = AerFiltro ; Aer2 = AerFiltro)),
             ((var(ClaFiltro) ; Cla1 = ClaFiltro ; Cla2 = ClaFiltro))
            ),
            Vuelos).
% Búsqueda de vuelos con dos escalas que cumplen los filtros
buscar_vuelos_con_dos_escalas(Origen, Destino, TipoFiltro, AerFiltro, ClaFiltro, Pres, Vuelos) :-
    findall([Origen, [Escala1, Aer1, Num1, Dur1, Cos1], [Escala2, Aer2, Num2, Dur2, Cos2], [Destino, Aer3, Num3, Dur3, Cos3]],
            (arista_filtrada(Origen, Escala1, vuelo(Num1, Aer1, Cla1, Cos1, Dur1, _), TipoFiltro, AerFiltro, ClaFiltro, _),
             arista_filtrada(Escala1, Escala2, vuelo(Num2, Aer2, Cla2, Cos2, Dur2, _), TipoFiltro, AerFiltro, ClaFiltro, _),
             arista_filtrada(Escala2, Destino, vuelo(Num3, Aer3, Cla3, Cos3, Dur3, _), TipoFiltro, AerFiltro, ClaFiltro, _),
             % Asegurarnos que las escalas no repitan aeropuertos
             Escala1 \= Destino, Escala1 \= Origen, Escala1 \= Escala2,
             Escala2 \= Destino, Escala2 \= Origen,
             % Verificación conjunta del presupuesto
             CostoTotal is Cos1 + Cos2 + Cos3,
             (var(Pres) ; (number(Pres), CostoTotal =< Pres)),
             % Para flexibilidad, permitimos que al menos uno de los vuelos cumpla con preferencias específicas
             ((var(AerFiltro) ; Aer1 = AerFiltro ; Aer2 = AerFiltro ; Aer3 = AerFiltro)),
             ((var(ClaFiltro) ; Cla1 = ClaFiltro ; Cla2 = ClaFiltro ; Cla3 = ClaFiltro))
            ),
            Vuelos).

% Búsqueda de vuelos con tres escalas que cumplen los filtros
buscar_vuelos_con_tres_escalas(Origen, Destino, TipoFiltro, AerFiltro, ClaFiltro, Pres, Vuelos) :-
    findall([Origen, [Escala1, Aer1, Num1, Dur1, Cos1], [Escala2, Aer2, Num2, Dur2, Cos2],
             [Escala3, Aer3, Num3, Dur3, Cos3], [Destino, Aer4, Num4, Dur4, Cos4]],
            (arista_filtrada(Origen, Escala1, vuelo(Num1, Aer1, Cla1, Cos1, Dur1, _), TipoFiltro, AerFiltro, ClaFiltro, _),
             arista_filtrada(Escala1, Escala2, vuelo(Num2, Aer2, Cla2, Cos2, Dur2, _), TipoFiltro, AerFiltro, ClaFiltro, _),
             arista_filtrada(Escala2, Escala3, vuelo(Num3, Aer3, Cla3, Cos3, Dur3, _), TipoFiltro, AerFiltro, ClaFiltro, _),
             arista_filtrada(Escala3, Destino, vuelo(Num4, Aer4, Cla4, Cos4, Dur4, _), TipoFiltro, AerFiltro, ClaFiltro, _),
             % Asegurarnos que las escalas no repitan aeropuertos
             Escala1 \= Destino, Escala1 \= Origen, Escala1 \= Escala2, Escala1 \= Escala3,
             Escala2 \= Destino, Escala2 \= Origen, Escala2 \= Escala3,
             Escala3 \= Destino, Escala3 \= Origen,
             % Verificación conjunta del presupuesto
             CostoTotal is Cos1 + Cos2 + Cos3 + Cos4,
             (var(Pres) ; (number(Pres), CostoTotal =< Pres)),
             % Para flexibilidad, permitimos que al menos uno de los vuelos cumpla con preferencias específicas
             ((var(AerFiltro) ; Aer1 = AerFiltro ; Aer2 = AerFiltro ; Aer3 = AerFiltro ; Aer4 = AerFiltro)),
             ((var(ClaFiltro) ; Cla1 = ClaFiltro ; Cla2 = ClaFiltro ; Cla3 = ClaFiltro ; Cla4 = ClaFiltro))
            ),
            Vuelos).
% Ordenar por duración
ordenar_por_duracion(VuelosDirectos, VuelosOrdenados) :-
    predsort(comparar_duracion, VuelosDirectos, VuelosOrdenados).

% Ordenar por duración total para vuelos con dos escalas
ordenar_por_duracion_dos_escalas(VuelosEscala, VuelosOrdenados) :-
    predsort(comparar_duracion_dos_escalas, VuelosEscala, VuelosOrdenados).

% Ordenar por duración total para vuelos con tres escalas
ordenar_por_duracion_tres_escalas(VuelosEscala, VuelosOrdenados) :-
    predsort(comparar_duracion_tres_escalas, VuelosEscala, VuelosOrdenados).

% Comparador de duración para vuelos directos
comparar_duracion(Orden, [_, [_, _, _, Dur1, _]], [_, [_, _, _, Dur2, _]]) :-
    (Dur1 < Dur2 -> Orden = (<) ; Orden = (>)).

% Comparador de duración para vuelos con dos escalas
comparar_duracion_dos_escalas(Orden,
    [_, [_, _, _, Dur1, _], [_, _, _, Dur2, _], [_, _, _, Dur3, _]],
    [_, [_, _, _, Dur4, _], [_, _, _, Dur5, _], [_, _, _, Dur6, _]]) :-
    DurTotal1 is Dur1 + Dur2 + Dur3,
    DurTotal2 is Dur4 + Dur5 + Dur6,
    (DurTotal1 < DurTotal2 -> Orden = (<) ; Orden = (>)).

% Comparador de duración para vuelos con tres escalas
comparar_duracion_tres_escalas(Orden,
    [_, [_, _, _, Dur1, _], [_, _, _, Dur2, _], [_, _, _, Dur3, _], [_, _, _, Dur4, _]],
    [_, [_, _, _, Dur5, _], [_, _, _, Dur6, _], [_, _, _, Dur7, _], [_, _, _, Dur8, _]]) :-
    DurTotal1 is Dur1 + Dur2 + Dur3 + Dur4,
    DurTotal2 is Dur5 + Dur6 + Dur7 + Dur8,
    (DurTotal1 < DurTotal2 -> Orden = (<) ; Orden = (>)).

% Ordenar por duración total para vuelos con escala
ordenar_por_duracion_escala(VuelosEscala, VuelosOrdenados) :-
    predsort(comparar_duracion_escala, VuelosEscala, VuelosOrdenados).

% Comparador de duración para vuelos con escala
comparar_duracion_escala(Orden,
    [_, [_, _, _, Dur1, _], [_, _, _, Dur2, _]],
    [_, [_, _, _, Dur3, _], [_, _, _, Dur4, _]]) :-
    DurTotal1 is Dur1 + Dur2,
    DurTotal2 is Dur3 + Dur4,
    (DurTotal1 < DurTotal2 -> Orden = (<) ; Orden = (>)).

% BÚSQUEDA PRINCIPAL DE VUELOS  -----------------------------------------

buscar_vuelo :-
    origen(O), destino(D),
    normalizar_filtros(TipoFiltro, AerolineaPref, ClasePref, Pres, PriorizarDuracion),

    writeln("Buscando vuelos..."),

    % Buscar vuelos directos
    buscar_vuelos_directos(O, D, TipoFiltro, AerolineaPref, ClasePref, Pres, VuelosDirectosNoOrdenados),

    % Ordenar por duración si corresponde
    (PriorizarDuracion = si ->
        ordenar_por_duracion(VuelosDirectosNoOrdenados, VuelosDirectos)
    ;
        VuelosDirectos = VuelosDirectosNoOrdenados
    ),

    % Verificar si encontramos vuelos directos
    (VuelosDirectos \= [] ->
        writeln("¡Encontré vuelos directos que cumplen con sus requisitos!"),
        % Seleccionamos el mejor vuelo
        VuelosDirectos = [[Origen, [Destino, Aero, NumVuelo, Duracion, Costo]] | _],

        % Obtener nombres completos de los aeropuertos para mejor presentación
        aeropuerto(NombreOrigen, Origen),
        aeropuerto(NombreDestino, Destino),

        (PriorizarDuracion = si ->
            format("Su vuelo más rápido sería: ", [])
        ;
            format("Su vuelo recomendado sería: ", [])
        ),

        % Presentación detallada para el usuario
        format("volar desde ~w (~w) hasta ~w (~w) en el vuelo ~w de ~w, con una duración de ~w horas y un costo de $~w.~n",
               [NombreOrigen, Origen, NombreDestino, Destino, NumVuelo, Aero, Duracion, Costo]),

        writeln("Gracias por usar TravelAgencyLog.")
    ;
        writeln("No hay vuelos directos disponibles, buscando vuelos con escala..."),
        % Buscar vuelos con una escala
        buscar_vuelos_con_escala(O, D, TipoFiltro, AerolineaPref, ClasePref, Pres, VuelosEscalaNoOrdenados),

        % Ordenar por duración total si corresponde
        (PriorizarDuracion = si ->
            ordenar_por_duracion_escala(VuelosEscalaNoOrdenados, VuelosEscala)
        ;
            VuelosEscala = VuelosEscalaNoOrdenados
        ),

        % Verificar si encontramos vuelos con una escala
        (VuelosEscala \= [] ->
            writeln("¡Encontré vuelos con una escala que cumplen con sus requisitos!"),
            VuelosEscala = [[Origen, [Escala, Aero1, NumVuelo1, Duracion1, Costo1], [Destino, Aero2, NumVuelo2, Duracion2, Costo2]] | _],

            % Obtener nombres completos de los aeropuertos para mejor presentación
            aeropuerto(NombreOrigen, Origen),
            aeropuerto(NombreEscala, Escala),
            aeropuerto(NombreDestino, Destino),

            CostoTotal is Costo1 + Costo2,
            DuracionTotal is Duracion1 + Duracion2,

            (PriorizarDuracion = si ->
                format("Su ruta más rápida sería: ",[])
            ;
                format("Su ruta recomendada sería: ", [])
            ),

            % Presentación detallada para el usuario
            format("volar desde ~w (~w) hasta ~w (~w) en el vuelo ~w de ~w, con una duración de ~w horas y un costo de $~w,~n",
                   [NombreOrigen, Origen, NombreEscala, Escala, NumVuelo1, Aero1, Duracion1, Costo1]),
            format("y luego desde ~w (~w) hasta ~w (~w) en el vuelo ~w de ~w, con una duración de ~w horas y un costo de $~w.~n",
                   [NombreEscala, Escala, NombreDestino, Destino, NumVuelo2, Aero2, Duracion2, Costo2]),
            format("Duración total: ~w horas. Costo total: $~w.~n", [DuracionTotal, CostoTotal]),

            writeln("Gracias por usar TravelAgencyLog.")
        ;
            writeln("No hay vuelos con una escala disponibles, buscando vuelos con dos escalas..."),
            % Buscar vuelos con dos escalas
            buscar_vuelos_con_dos_escalas(O, D, TipoFiltro, AerolineaPref, ClasePref, Pres, VuelosDosEscalasNoOrdenados),

            % Ordenar por duración total si corresponde
            (PriorizarDuracion = si ->
                ordenar_por_duracion_dos_escalas(VuelosDosEscalasNoOrdenados, VuelosDosEscalas)
            ;
                VuelosDosEscalas = VuelosDosEscalasNoOrdenados
            ),

            % Verificar si encontramos vuelos con dos escalas
            (VuelosDosEscalas \= [] ->
                writeln("¡Encontré vuelos con dos escalas que cumplen con sus requisitos!"),
                VuelosDosEscalas = [[Origen, [Escala1, Aero1, NumVuelo1, Duracion1, Costo1],
                                     [Escala2, Aero2, NumVuelo2, Duracion2, Costo2],
                                     [Destino, Aero3, NumVuelo3, Duracion3, Costo3]] | _],

                % Obtener nombres completos de los aeropuertos para mejor presentación
                aeropuerto(NombreOrigen, Origen),
                aeropuerto(NombreEscala1, Escala1),
                aeropuerto(NombreEscala2, Escala2),
                aeropuerto(NombreDestino, Destino),

                CostoTotal is Costo1 + Costo2 + Costo3,
                DuracionTotal is Duracion1 + Duracion2 + Duracion3,

                (PriorizarDuracion = si ->
                    format("Su ruta más rápida sería: ",[])
                ;
                    format("Su ruta recomendada sería: ", [])
                ),

                % Presentación detallada para el usuario
                format("volar desde ~w (~w) hasta ~w (~w) en el vuelo ~w de ~w, con una duración de ~w horas y un costo de $~w,~n",
                       [NombreOrigen, Origen, NombreEscala1, Escala1, NumVuelo1, Aero1, Duracion1, Costo1]),
                format("luego desde ~w (~w) hasta ~w (~w) en el vuelo ~w de ~w, con una duración de ~w horas y un costo de $~w,~n",
                       [NombreEscala1, Escala1, NombreEscala2, Escala2, NumVuelo2, Aero2, Duracion2, Costo2]),
                format("y finalmente desde ~w (~w) hasta ~w (~w) en el vuelo ~w de ~w, con una duración de ~w horas y un costo de $~w.~n",
                       [NombreEscala2, Escala2, NombreDestino, Destino, NumVuelo3, Aero3, Duracion3, Costo3]),
                format("Duración total: ~w horas. Costo total: $~w.~n", [DuracionTotal, CostoTotal]),

                writeln("Gracias por usar TravelAgencyLog.")
            ;
                writeln("No hay vuelos con dos escalas disponibles, buscando vuelos con tres escalas..."),
                % Buscar vuelos con tres escalas
                buscar_vuelos_con_tres_escalas(O, D, TipoFiltro, AerolineaPref, ClasePref, Pres, VuelosTresEscalasNoOrdenados),

                % Ordenar por duración total si corresponde
                (PriorizarDuracion = si ->
                    ordenar_por_duracion_tres_escalas(VuelosTresEscalasNoOrdenados, VuelosTresEscalas)
                ;
                    VuelosTresEscalas = VuelosTresEscalasNoOrdenados
                ),

                % Verificar si encontramos vuelos con tres escalas
                (VuelosTresEscalas \= [] ->
                    writeln("¡Encontré vuelos con tres escalas que cumplen con sus requisitos!"),
                    VuelosTresEscalas = [[Origen, [Escala1, Aero1, NumVuelo1, Duracion1, Costo1],
                                         [Escala2, Aero2, NumVuelo2, Duracion2, Costo2],
                                         [Escala3, Aero3, NumVuelo3, Duracion3, Costo3],
                                         [Destino, Aero4, NumVuelo4, Duracion4, Costo4]] | _],

                    % Obtener nombres completos de los aeropuertos para mejor presentación
                    aeropuerto(NombreOrigen, Origen),
                    aeropuerto(NombreEscala1, Escala1),
                    aeropuerto(NombreEscala2, Escala2),
                    aeropuerto(NombreEscala3, Escala3),
                    aeropuerto(NombreDestino, Destino),

                    CostoTotal is Costo1 + Costo2 + Costo3 + Costo4,
                    DuracionTotal is Duracion1 + Duracion2 + Duracion3 + Duracion4,

                    (PriorizarDuracion = si ->
                        format("Su ruta más rápida sería: ",[])
                    ;
                        format("Su ruta recomendada sería: ", [])
                    ),

                    % Presentación detallada para el usuario
                    format("volar desde ~w (~w) hasta ~w (~w) en el vuelo ~w de ~w, con una duración de ~w horas y un costo de $~w,~n",
                           [NombreOrigen, Origen, NombreEscala1, Escala1, NumVuelo1, Aero1, Duracion1, Costo1]),
                    format("luego desde ~w (~w) hasta ~w (~w) en el vuelo ~w de ~w, con una duración de ~w horas y un costo de $~w,~n",
                           [NombreEscala1, Escala1, NombreEscala2, Escala2, NumVuelo2, Aero2, Duracion2, Costo2]),
                    format("después desde ~w (~w) hasta ~w (~w) en el vuelo ~w de ~w, con una duración de ~w horas y un costo de $~w,~n",
                           [NombreEscala2, Escala2, NombreEscala3, Escala3, NumVuelo3, Aero3, Duracion3, Costo3]),
                    format("y finalmente desde ~w (~w) hasta ~w (~w) en el vuelo ~w de ~w, con una duración de ~w horas y un costo de $~w.~n",
                           [NombreEscala3, Escala3, NombreDestino, Destino, NumVuelo4, Aero4, Duracion4, Costo4]),
                    format("Duración total: ~w horas. Costo total: $~w.~n", [DuracionTotal, CostoTotal]),

                    writeln("Gracias por usar TravelAgencyLog.")
                ;
                    writeln("Lamentablemente no tenemos un vuelo que se ajuste a sus necesidades. Muchas gracias por utilizar TravelAgencyLog.")
                )
            )
        )
    ).

