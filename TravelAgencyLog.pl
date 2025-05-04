% Definicion de base de datos -----------------------------------------
% Hechos: vuelo(Numero, Origen, Destino, Aerolinea, Clase, Costo, Duracion, Tipo).
vuelo(cm404, sjo, pty, avianca, negocios, 500, 1, comercial).
vuelo(cm405, sjo, pty, copa, negocios, 1500, 1, charter).
vuelo(cm406, sjo, pty, american, economica, 500, 1, comercial).
vuelo(ua105, pty, phx, united, economica, 900, 6, comercial).
vuelo(ua106, pty, phx, united, negocios, 1900, 6, charter).
vuelo(ua107, pty, phx, copa, economica, 900, 6, comercial).
vuelo(ta321, sjo, mia, avianca, economica, 450, 3, comercial).
vuelo(aa876, mia, phx, american, negocios, 1300, 5, comercial).
vuelo(ch101, sjo, phx, charter_air, economica, 1100, 7, charter).
vuelo(ch202, sjo, pty, charter_air, negocios, 1200, 1, charter).
vuelo(ua777, pty, phx, united, negocios, 1500, 6, comercial).
vuelo(lh450, sjo, fra, lufthansa, negocios, 2000, 11, comercial).
vuelo(fra203, fra, phx, united, economica, 1300, 12, comercial).
vuelo(am888, sjo, mex, aeromexico, economica, 400, 2, comercial).
vuelo(cp201, sjo, bog, copa, economica, 350, 2, comercial).
vuelo(av450, sjo, bog, avianca, negocios, 550, 2, comercial).
vuelo(la302, sjo, lim, latam, economica, 480, 3, comercial).
vuelo(la303, sjo, lim, latam, negocios, 780, 3, comercial).
vuelo(aa201, sjo, dfw, american, economica, 650, 4, comercial).
vuelo(ac100, sjo, yyz, air_canada, negocios, 900, 5, comercial).

% Vuelos desde aeropuertos intermedios a destinos finales
vuelo(av451, bog, mia, avianca, economica, 400, 3, comercial).
vuelo(av452, bog, mia, avianca, negocios, 800, 3, comercial).
vuelo(la305, lim, phx, latam, economica, 750, 7, comercial).
vuelo(la306, lim, phx, latam, negocios, 1400, 7, comercial).
vuelo(aa202, dfw, phx, american, economica, 300, 2, comercial).
vuelo(aa203, dfw, phx, american, negocios, 600, 2, comercial).
vuelo(ac101, yyz, phx, air_canada, economica, 450, 4, comercial).

% Vuelos desde mia a otros destinos
vuelo(dl401, mia, atl, delta, economica, 200, 2, comercial).
vuelo(dl402, mia, atl, delta, negocios, 500, 2, comercial).
vuelo(dl403, atl, phx, delta, economica, 300, 3, comercial).
vuelo(dl404, atl, phx, delta, negocios, 700, 3, comercial).

% Vuelos desde mex a otros destinos
vuelo(am889, mex, lax, aeromexico, economica, 350, 3, comercial).
vuelo(am890, mex, phx, aeromexico, economica, 320, 3, comercial).
vuelo(am891, mex, phx, aeromexico, negocios, 720, 3, comercial).

% Vuelos chárter adicionales
vuelo(ch103, sjo, bog, charter_air, negocios, 800, 2, charter).
vuelo(ch104, bog, phx, charter_air, negocios, 1500, 8, charter).
vuelo(ch105, sjo, mex, charter_air, economica, 750, 2, charter).
vuelo(pr100, pty, lim, priority_air, negocios, 900, 3, charter).
vuelo(pr101, lim, fra, priority_air, negocios, 2200, 12, charter).

% Conexiones europeas adicionales
vuelo(lh451, fra, mad, lufthansa, economica, 200, 2, comercial).
vuelo(lh452, mad, mia, lufthansa, economica, 800, 9, comercial).
vuelo(ba100, lhr, phx, british_airways, negocios, 1700, 10, comercial).
vuelo(af200, cdg, mia, air_france, economica, 900, 8, comercial).
vuelo(af201, sjo, cdg, air_france, negocios, 1800, 10, comercial).

% aeropuerto(NombreLargo, Codigo).
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

% Función para ordenar vuelos por duración
ordenar_por_duracion(VuelosIn, VuelosOut) :-
    predsort(comparar_duracion, VuelosIn, VuelosOut).

% Comparador para ordenar vuelos directos por duración
comparar_duracion(Orden, [_, _, _, _, _, _, Dur1], [_, _, _, _, _, _, Dur2]) :-
    (Dur1 < Dur2 -> Orden = (<) ; Orden = (>)).

% Comparador para ordenar vuelos con escala por duración total
comparar_duracion_escala(Orden,
    [_, _, _, _, _, _, Dur1, _, _, _, _, _, _, Dur2],
    [_, _, _, _, _, _, Dur3, _, _, _, _, _, _, Dur4]) :-
    DurTotal1 is Dur1 + Dur2,
    DurTotal2 is Dur3 + Dur4,
    (DurTotal1 < DurTotal2 -> Orden = (<) ; Orden = (>)).

buscar_vuelo :-
    origen(O), destino(D),

    normalizar_filtros(TipoFiltro, AerolineaPref, ClasePref, Pres, PriorizarDuracion),

    % VUELOS DIRECTOS
    findall([Num, O, D, Aer, Cla, Cos, Dur],
        ( vuelo(Num, O, D, Aer, Cla, Cos, Dur, TipoVuelo),
          % Evaluar tipo de vuelo
          (TipoFiltro == _ ; TipoVuelo = TipoFiltro),
          % Evaluar clase
          (ClasePref == _ ; Cla = ClasePref),
          % Evaluar aerolínea
          (AerolineaPref == _ ; Aer = AerolineaPref),
          % Evaluar presupuesto
          (var(Pres) ; (number(Pres), Cos =< Pres))
        ),
        VuelosDirectosNoOrdenados),

    % Ordenar por duración si corresponde
    (PriorizarDuracion = si ->
        ordenar_por_duracion(VuelosDirectosNoOrdenados, VuelosDirectos)
    ;
        VuelosDirectos = VuelosDirectosNoOrdenados
    ),

    writeln("Buscando vuelos..."),

    ( VuelosDirectos \= [] ->
        writeln("¡Encontré vuelos directos que cumplen con sus requisitos!"),
        % Seleccionamos el primer vuelo (el de menor duración si PriorizarDuracion = si)
        VuelosDirectos = [[Num1, O1, D1, Aer1, Cla1, Cos1, Dur1] | _],
        (PriorizarDuracion = si ->
            format("Su vuelo más rápido sería el ~w de ~w a ~w con ~w en clase ~w por ~w dólares, duración ~w horas. Gracias por usar TravelAgencyLog.~n",
                   [Num1, O1, D1, Aer1, Cla1, Cos1, Dur1])
        ;
            format("Su vuelo sería el ~w de ~w a ~w con ~w en clase ~w por ~w dólares, duración ~w horas. Gracias por usar TravelAgencyLog.~n",
                   [Num1, O1, D1, Aer1, Cla1, Cos1, Dur1])
        )
    ;
        writeln("No hay vuelos directos disponibles, buscando vuelos con escala..."),
        % VUELOS CON ESCALA - VERSIÓN MODIFICADA
        findall([Num1, O, M, Aer1, Cla1, Cos1, Dur1, Num2, M, D, Aer2, Cla2, Cos2, Dur2],
            ( vuelo(Num1, O, M, Aer1, Cla1, Cos1, Dur1, TipoVuelo1),
              vuelo(Num2, M, D, Aer2, Cla2, Cos2, Dur2, TipoVuelo2),
              M \= D, M \= O,  % Asegurarnos que la escala no sea ni origen ni destino

              % Evaluar tipo de vuelo
              ((TipoFiltro == _) -> true ;
                (TipoVuelo1 = TipoFiltro, TipoVuelo2 = TipoFiltro)),

              % Evaluar clase - modificado para ser más flexible
              ((ClasePref == _) -> true ;
                (Cla1 = ClasePref ; Cla2 = ClasePref)),

              % Evaluar aerolínea - modificado para ser más flexible
              ((AerolineaPref == _) -> true ;
                (Aer1 = AerolineaPref ; Aer2 = AerolineaPref)),

              % Evaluar presupuesto
              TotalCost is Cos1 + Cos2,
              (var(Pres) ; (number(Pres), TotalCost =< Pres))
            ),
            VuelosConEscalaNoOrdenados),

        % Ordenar vuelos con escala por duración total si corresponde
        (PriorizarDuracion = si ->
            predsort(comparar_duracion_escala, VuelosConEscalaNoOrdenados, VuelosConEscala)
        ;
            VuelosConEscala = VuelosConEscalaNoOrdenados
        ),

        ( VuelosConEscala = [] ->
            writeln("Lamentablemente no tenemos un vuelo que se ajuste a sus necesidades. Muchas gracias por utilizar TravelAgencyLog.")
        ;
            VuelosConEscala = [[Num1, O1, M1, Aer1, Cla1, Cos1, Dur1, Num2, _, D1, Aer2, Cla2, Cos2, Dur2] | _],
            TotalCost is Cos1 + Cos2,
            DurTotal is Dur1 + Dur2,
            (PriorizarDuracion = si ->
                format("Su vuelo más rápido sería: ~w de ~w a ~w con ~w en clase ~w (duración ~w h), y luego ~w de ~w a ~w con ~w en clase ~w (duración ~w h). Costo total: $~w. Duración total: ~w h. Gracias por usar TravelAgencyLog.~n",
                    [Num1, O1, M1, Aer1, Cla1, Dur1, Num2, M1, D1, Aer2, Cla2, Dur2, TotalCost, DurTotal])
            ;
                format("Su vuelo sería: ~w de ~w a ~w con ~w en clase ~w (duración ~w h), y luego ~w de ~w a ~w con ~w en clase ~w (duración ~w h). Costo total: $~w. Duración total: ~w h. Gracias por usar TravelAgencyLog.~n",
                    [Num1, O1, M1, Aer1, Cla1, Dur1, Num2, M1, D1, Aer2, Cla2, Dur2, TotalCost, DurTotal])
            )
        )
    ).
