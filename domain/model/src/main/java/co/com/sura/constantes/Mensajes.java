package co.com.sura.constantes;


import lombok.Getter;

@Getter
public enum Mensajes {
    PETICION_EXITOSA("Solicitud exitosa"),
    PETICION_FALLIDA("Error en petición"),

    REMISION_CREADA("Se ha creado la remisión exitosamente"),
    REMISION_ACTUALIZADA("Se ha creado la novedad exitosamente"),
    REMISION_EXISTENTE("Ya existe una remisión con el id ?"),
    REMISION_NO_EXISTENTE("No existe o ya fue egresada la remisión con el id ?"),
    REMISION_CITAS_PROGRESO("la remisión con el id ? tiene citas confirmadas o en progreso"),
    ERROR_CREAR_REMISION("error al crear remisión"),
    ERROR_ACTUALIZAR_REMISION("error al actualizar remisión"),

    REMISION_EGRESADA("Se ha egresado la remisión"),
    ERROR_EGRESAR_REMISION("error a egresar remisión"),

    //agenda
    TURNO_DESAGENDADO("Se desagendó el turno completo"),
    ERROR_TURNO_DESAGENDADO("Error al desagendar turno completo"),

    TURNO_AUTOAGENDADO("Se programó el turno exitosamente,  validar si aún quedan citas pendientes por agendar"),
    ERROR_AUTOAGENDADO("Error al autoagendar turno"),

    SE_NOTIFICO_FARMACIA("Se ha notificado a farmacia exitosamente"),
    NO_NOTIFICO_FARMACIA("Se presentó un error al notificar a farmacia"),

    SE_ASIGNO_PROFESIONAL_CITA("Se ha agendado la cita  al profesional exitosamente"),
    NO_ASIGNO_PROFESIONAL_CITA("Se presentó un error y no se pudo agendar la cita"),

    SE_DESASIGNO_PROFESIONAL_CITA("Se ha desagendado la cita  al profesional exitosamente"),
    NO_DESASIGNO_PROFESIONAL_CITA("Se presentó un error y no se pudo desagendar la cita"),

    SE_REPROGRAMO_HORA_CITA("Se ha reprogramado hora la hora de la cita exitosamente"),
    NO_REPROGRAMO_HORA_CITA("Se presentó un error y no se pudo reprogramar hora de la cita"),

    SE_ASIGNO_PROFESIONAL_TURNO("Se ha agendado el profesional al turno  ? exitosamente"),
    NO_ASIGNO_PROFESIONAL_TURNO("Se presentó un error y no se pudo asignar el profesional al turno"),

    SE_DESASIGNO_PROFESIONAL_TURNO("Se ha desagendado el profesional al turno  ? exitosamente"),
    NO_DESASIGNO_PROFESIONAL_TURNO("Se presentó un error y no se pudo desasignar el profesional al turno"),

    SE_CALCULA_DESPLAZAMIENTO_CITAS_PROFESIONAL(
            "Se ha actualizó desplazamientos en turno de profesional exitosamente"),
    ERROR_CALCULAR_DESPLAZAMIENTO_CITAS_PROFESIONAL(
            "Se presentó un error y no se pudo actualizar desplazamientos en turno de profesional"),

   //profesionales
    PROFESIONAL_YA_EXISTE("Error al crear profesional, ya existe profesional"),
    PROFESIONAL_NO_EXISTE("Error al actualizar, el profesional no existe profesional"),
    SE_CREA_PROFESIONAL("Se ha creado profesional exitoxamente !!"),
    SE_ACTUALIZA_PROFESIONAL("Se ha actualizado profesional exitoxamente !!"),
    ERROR_CREAR_PROFESIONAL("Error al crear al profesional"),
    ERROR_ACTTUALIZAR_PROFESIONAL("Error al actualizar profesional"),

    //conductores
    CONDUCTOR_YA_EXISTE("Error al crear conductor, ya existe conductor"),
    CONDUCTOR_NO_EXISTE("Error al actualizar, el conductor no existe conductor"),
    SE_CREA_CONDUCTOR("Se ha creado conductor exitoxamente !!"),
    SE_ACTUALIZA_CONDUCTOR("Se ha actualizado conductor exitoxamente !!"),
    ERROR_CREAR_CONDUCTOR("Error al crear al conductor"),
    ERROR_ACTUALIZAR_CONDUCTOR("Error al actualizar conductor"),

    //moviles
    MOVIL_YA_EXISTE("Error al crear movil, ya existe movil"),
    MOVIL_NO_EXISTE("Error al actualizar, el movil no existe movil"),
    SE_CREA_MOVIL("Se ha creado movil exitoxamente !!"),
    SE_ACTUALIZA_MOVIL("Se ha actualizado movil exitoxamente !!"),
    ERROR_CREAR_MOVIL("Error al crear al movil"),
    ERROR_ACTUALIZAR_MOVIL("Error al actualizar movil"),

    //turnos profesional
    SE_ACTUALIZA_TURNO_PROFESIONAL("Se ha actualizado el  turno del profesional"),
    ERROR_ACTUALIZAR_TURNO_PROFESIONAL("Error al actualizar el turno del profesional"),

    RESPUESTA_TURNO("El profesional tiene citas asignadas");


    private final String value;


    Mensajes(String value) {
        this.value = value;
    }
}

