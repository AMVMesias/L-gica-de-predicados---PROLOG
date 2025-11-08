% ===========================================================================
% SISTEMA EXPERTO PARA DIAGNOSTICO DE OTITIS EN PROLOG
% Basado en criterios medicos profesionales
% ===========================================================================

% Limpiar la base de datos al inicio
:- dynamic(sintoma/1).
:- dynamic(factor_riesgo/1).
:- dynamic(examen_fisico/1).

% ===========================================================================
% DEFINICION DE SINTOMAS PRIMARIOS
% ===========================================================================

% Sintomas auditivos
sintoma_auditivo(dolor_oido).
sintoma_auditivo(sensacion_oido_tapado).
sintoma_auditivo(perdida_auditiva_temporal).
sintoma_auditivo(tinnitus).
sintoma_auditivo(vertigo).

% Sintomas de infeccion
sintoma_infeccioso(fiebre).
sintoma_infeccioso(escalofrios).
sintoma_infeccioso(malestar_general).
sintoma_infeccioso(fatiga).

% Sintomas de secrecion
sintoma_secrecion(otorrea_purulenta).
sintoma_secrecion(otorrea_sanguinolenta).
sintoma_secrecion(secrecion_clara).

% Sintomas neurologicos
sintoma_neurologico(cefalea).
sintoma_neurologico(irritabilidad).
sintoma_neurologico(dificultad_concentracion).

% ===========================================================================
% FACTORES DE RIESGO
% ===========================================================================

factor_riesgo(edad_pediatrica).      % < 5 anos
factor_riesgo(resfriado_reciente).   % ultimos 7 dias
factor_riesgo(alergias_respiratorias).
factor_riesgo(exposicion_humo_tabaco).
factor_riesgo(guarderia_escuela).
factor_riesgo(uso_chupete).
factor_riesgo(lactancia_artificial).
factor_riesgo(malformacion_paladar).
factor_riesgo(inmunodeficiencia).

% ===========================================================================
% HALLAZGOS EN EXAMEN FÍSICO
% ===========================================================================

examen_fisico(timpano_eritematoso).
examen_fisico(timpano_abombado).
examen_fisico(timpano_perforado).
examen_fisico(nivel_hidroaereo).
examen_fisico(movilidad_reducida_timpano).
examen_fisico(adenopatias_cervicales).
examen_fisico(secrecion_conducto_auditivo).

% ===========================================================================
% REGLAS DE DIAGNOSTICO PRINCIPAL
% ===========================================================================

% OTITIS MEDIA AGUDA (OMA)
otitis_media_aguda :-
    sintoma(dolor_oido),
    sintoma(fiebre),
    (examen_fisico(timpano_eritematoso); examen_fisico(timpano_abombado)),
    sintoma(sensacion_oido_tapado).

% OTITIS MEDIA AGUDA SEVERA
otitis_media_aguda_severa :-
    otitis_media_aguda,
    sintoma(fiebre),
    temperatura_alta,
    (sintoma(otorrea_purulenta); examen_fisico(timpano_perforado)).

% OTITIS MEDIA CON EFUSION (OME)
otitis_media_con_efusion :-
    sintoma(sensacion_oido_tapado),
    sintoma(perdida_auditiva_temporal),
    examen_fisico(nivel_hidroaereo),
    \+ sintoma(dolor_oido),
    \+ sintoma(fiebre).

% OTITIS EXTERNA AGUDA
otitis_externa_aguda :-
    sintoma(dolor_oido),
    dolor_aumenta_traccion_oreja,
    examen_fisico(secrecion_conducto_auditivo),
    \+ examen_fisico(timpano_eritematoso).

% OTITIS EXTERNA MALIGNA
otitis_externa_maligna :-
    otitis_externa_aguda,
    sintoma(cefalea),
    paralisis_facial,
    diabetes_mellitus.

% ===========================================================================
% REGLAS AUXILIARES PARA SEVERIDAD
% ===========================================================================

temperatura_alta :-
    sintoma(fiebre),
    (sintoma(escalofrios); sintoma(malestar_general)).

dolor_aumenta_traccion_oreja :-
    sintoma(dolor_oido),
    dolor_severo_manipulacion.

% Criterios de complicación
complicacion_potencial :-
    (sintoma(cefalea), sintoma(fiebre));
    sintoma(vertigo);
    sintoma(otorrea_sanguinolenta);
    paralisis_facial.

% ===========================================================================
% REGLAS DE PROBABILIDAD Y SEVERIDAD
% ===========================================================================

% Probabilidad alta de OMA
probabilidad_alta_oma :-
    sintoma(dolor_oido),
    sintoma(fiebre),
    factor_riesgo(edad_pediatrica),
    factor_riesgo(resfriado_reciente).

% Requiere antibióticos
requiere_antibioticos :-
    otitis_media_aguda_severa;
    (otitis_media_aguda, factor_riesgo(edad_pediatrica));
    otitis_externa_maligna.

% Seguimiento necesario
requiere_seguimiento :-
    otitis_media_con_efusion;
    complicacion_potencial;
    \+ mejora_72_horas.

% ===========================================================================
% DIAGNOSTICO DIFERENCIAL
% ===========================================================================

% Descartar otras patologías
no_es_otitis :-
    \+ sintoma(dolor_oido),
    \+ sintoma(sensacion_oido_tapado),
    \+ examen_fisico(timpano_eritematoso),
    \+ examen_fisico(secrecion_conducto_auditivo).

% Posible patologia externa
patologia_externa :-
    sintoma(dolor_oido),
    dolor_aumenta_traccion_oreja,
    \+ sintoma(fiebre).

% Posible cerumen impactado
cerumen_impactado :-
    sintoma(sensacion_oido_tapado),
    sintoma(perdida_auditiva_temporal),
    \+ sintoma(dolor_oido),
    \+ sintoma(fiebre).

% ===========================================================================
% MOTOR DE DIAGNOSTICO PRINCIPAL
% ===========================================================================

diagnosticar_otitis :-
    write('=== DIAGNOSTICO DE OTITIS ==='), nl,
    (   otitis_media_aguda_severa ->
        write('DIAGNOSTICO: Otitis Media Aguda SEVERA'), nl,
        write('TRATAMIENTO: Antibioticos URGENTES'), nl,
        write('SEGUIMIENTO: 48-72 horas')
    ;   otitis_media_aguda ->
        write('DIAGNOSTICO: Otitis Media Aguda'), nl,
        (   requiere_antibioticos ->
            write('TRATAMIENTO: Antibioticos + Analgesicos')
        ;   write('TRATAMIENTO: Manejo sintomatico + observacion')
        )
    ;   otitis_media_con_efusion ->
        write('DIAGNOSTICO: Otitis Media con Efusion'), nl,
        write('TRATAMIENTO: Observacion + control audiologico')
    ;   otitis_externa_aguda ->
        write('DIAGNOSTICO: Otitis Externa Aguda'), nl,
        write('TRATAMIENTO: Antibioticos topicos')
    ;   otitis_externa_maligna ->
        write('DIAGNOSTICO: Otitis Externa Maligna'), nl,
        write('TRATAMIENTO: HOSPITALIZACION URGENTE')
    ;   cerumen_impactado ->
        write('DIAGNOSTICO: Impactacion de Cerumen'), nl,
        write('TRATAMIENTO: Lavado otico o extraccion')
    ;   no_es_otitis ->
        write('NO se evidencia otitis'), nl,
        write('Considerar otros diagnosticos diferenciales')
    ;   write('DIAGNOSTICO INCIERTO - Requiere evaluacion medica')
    ), nl,
    
    % Evaluacion de complicaciones
    (   complicacion_potencial ->
        nl, write('ALERTA: Posibles complicaciones - Derivar a especialista'), nl
    ;   true
    ),
    
    % Recomendaciones de seguimiento
    (   requiere_seguimiento ->
        nl, write('SEGUIMIENTO requerido en 3-7 dias'), nl
    ;   true
    ).

% ===========================================================================
% INTERFAZ INTERACTIVA PARA EVALUACION
% ===========================================================================

evaluar_paciente :-
    retractall(sintoma(_)),
    retractall(factor_riesgo(_)),
    retractall(examen_fisico(_)),
    
    write('=== EVALUACION CLINICA DE OTITIS ==='), nl, nl,
    
    % Recoleccion de sintomas
    write('SINTOMAS (responder si/no):'), nl,
    preguntar_sintoma(dolor_oido, 'Presenta dolor de oido?'),
    preguntar_sintoma(fiebre, 'Tiene fiebre?'),
    preguntar_sintoma(sensacion_oido_tapado, 'Siente el oido tapado?'),
    preguntar_sintoma(perdida_auditiva_temporal, 'Ha notado perdida auditiva?'),
    preguntar_sintoma(otorrea_purulenta, 'Sale pus del oido?'),
    preguntar_sintoma(cefalea, 'Tiene dolor de cabeza?'),
    preguntar_sintoma(vertigo, 'Presenta mareos o vertigo?'),
    
    % Factores de riesgo
    nl, write('FACTORES DE RIESGO:'), nl,
    preguntar_factor(edad_pediatrica, 'El paciente es menor de 5 anos?'),
    preguntar_factor(resfriado_reciente, 'Ha tenido resfriado en la ultima semana?'),
    
    % Examen fisico
    nl, write('EXAMEN FISICO (si es posible):'), nl,
    preguntar_examen(timpano_eritematoso, 'El timpano se ve rojo/inflamado?'),
    preguntar_examen(timpano_abombado, 'El timpano esta abombado?'),
    
    % Realizar diagnostico
    nl,
    diagnosticar_otitis.

preguntar_sintoma(Sintoma, Pregunta) :-
    write(Pregunta), write(' '),
    read(Respuesta),
    (   Respuesta = si ->
        assertz(sintoma(Sintoma))
    ;   true
    ).

preguntar_factor(Factor, Pregunta) :-
    write(Pregunta), write(' '),
    read(Respuesta),
    (   Respuesta = si ->
        assertz(factor_riesgo(Factor))
    ;   true
    ).

preguntar_examen(Hallazgo, Pregunta) :-
    write(Pregunta), write(' '),
    read(Respuesta),
    (   Respuesta = si ->
        assertz(examen_fisico(Hallazgo))
    ;   true
    ).

% ===========================================================================
% CASOS DE PRUEBA PREDEFINIDOS
% ===========================================================================

% Caso 1: OMA típica en niño
caso_oma_pediatrico :-
    retractall(sintoma(_)),
    retractall(factor_riesgo(_)),
    retractall(examen_fisico(_)),
    
    assertz(sintoma(dolor_oido)),
    assertz(sintoma(fiebre)),
    assertz(sintoma(sensacion_oido_tapado)),
    assertz(factor_riesgo(edad_pediatrica)),
    assertz(factor_riesgo(resfriado_reciente)),
    assertz(examen_fisico(timpano_eritematoso)),
    
    write('=== CASO CLINICO: OMA PEDIATRICA ==='), nl,
    diagnosticar_otitis.

% Caso 2: OME (Otitis serosa)
caso_ome :-
    retractall(sintoma(_)),
    retractall(factor_riesgo(_)),
    retractall(examen_fisico(_)),
    
    assertz(sintoma(sensacion_oido_tapado)),
    assertz(sintoma(perdida_auditiva_temporal)),
    assertz(examen_fisico(nivel_hidroaereo)),
    
    write('=== CASO CLINICO: OTITIS MEDIA CON EFUSION ==='), nl,
    diagnosticar_otitis.

% Caso 3: Otitis externa
caso_otitis_externa :-
    retractall(sintoma(_)),
    retractall(factor_riesgo(_)),
    retractall(examen_fisico(_)),
    
    assertz(sintoma(dolor_oido)),
    assertz(examen_fisico(secrecion_conducto_auditivo)),
    assertz(dolor_severo_manipulacion),
    
    write('=== CASO CLINICO: OTITIS EXTERNA ==='), nl,
    diagnosticar_otitis.

% ===========================================================================
% PREDICADOS DE UTILIDAD
% ===========================================================================

% Mostrar ayuda
ayuda :-
    write('=== SISTEMA EXPERTO DE DIAGNOSTICO DE OTITIS ==='), nl, nl,
    write('COMANDOS DISPONIBLES:'), nl,
    write('- evaluar_paciente.    -> Evaluacion interactiva completa'), nl,
    write('- diagnosticar_otitis. -> Diagnostico con datos actuales'), nl,
    write('- caso_oma_pediatrico. -> Caso de prueba: OMA pediatrica'), nl,
    write('- caso_ome.           -> Caso de prueba: OME'), nl,
    write('- caso_otitis_externa. -> Caso de prueba: Otitis externa'), nl,
    write('- ayuda.              -> Mostrar esta ayuda'), nl, nl,
    write('Para usar: escribir el comando seguido de punto y Enter'), nl.

% Mostrar ayuda al cargar
:- ayuda.

% ===========================================================================
% HECHOS ADICIONALES PARA CASOS ESPECÍFICOS
% ===========================================================================

% Predicados auxiliares que se pueden definir según el caso
dolor_severo_manipulacion.
mejora_72_horas :- fail.  % Por defecto, asumimos que no ha mejorado
paralisis_facial :- fail.  % Por defecto, no hay parálisis
diabetes_mellitus :- fail.  % Por defecto, no hay diabetes