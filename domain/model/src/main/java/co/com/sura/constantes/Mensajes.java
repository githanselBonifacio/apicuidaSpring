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

    QSL_EJEMPLO("SELECT paciente.numero_identificacion,paciente.tipo_identificacion,paciente.nombres,paciente.apellidos, " +
            " remision.id_remision, " +
            " tratamiento.* , " +
            " cita.fecha_programada " +
            " FROM paciente " +
            " INNER JOIN remision ON remision.numero_identificacion_paciente = paciente.numero_identificacion " +
            " INNER JOIN cita ON cita.id_remision = remision.id_remision " +
            " INNER JOIN tratamiento ON tratamiento.id_cita = cita.id_cita " +
            " WHERE cita.id_estado = 3 or cita.id_estado = 4 or cita.id_estado = 6 ;");


    private final String value;


    Mensajes(String value) {
        this.value = value;
    }
}

