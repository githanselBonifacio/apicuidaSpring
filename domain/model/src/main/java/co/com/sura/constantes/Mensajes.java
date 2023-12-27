package co.com.sura.constantes;


import lombok.Getter;

@Getter
public class Mensajes {
    public static  final String ERROR_TEST = "error test";
    public static final String ERROR_DEMASIADOS_TURNOS = "Demasiados turnos que procesar, intente con menos de %s" +
            " turnos usted solicita modificar %s turnos";
    public static final String PETICION_EXITOSA = "Solicitud exitosa";
    public static final String PETICION_FALLIDA ="Error en petición";

    public static final String REMISION_CREADA ="Se ha creado la remisión exitosamente";
    public static final String REMISION_ACTUALIZADA ="Se ha creado la novedad exitosamente";
    public static final String REMISION_EXISTENTE ="Ya existe una remisión con el id ?";
    public static final String REMISION_NO_EXISTENTE="No existe o ya fue egresada la remisión con el id ?";
    public static final String REMISION_CITAS_PROGRESO ="la remisión con el id ? tiene citas confirmadas o en progreso";
    public static final String ERROR_CREAR_REMISION ="error al crear remisión";
    public static final String ERROR_ACTUALIZAR_REMISION ="error al actualizar remisión";

    public static final String REMISION_EGRESADA = "Se ha egresado la remisión";
    public static final String ERROR_EGRESAR_REMISION ="error a egresar remisión";

    //agenda
    public static final String TURNO_DESAGENDADO ="Se desagendó el turno completo";
    public static final String ERROR_TURNO_DESAGENDADO ="Error al desagendar turno completo";
    public static final String ERROR_TURNO_DESAGENDADO_ESTADOS_CITAS = "No es posible desagendar el profesional porque tiene citas asignadas "+
            "en estado confirmada, en progreso o finalizada, por favor verifique la agenda";

    public static final String TURNO_AUTOAGENDADO ="Se programó el turno exitosamente,  validar si aún quedan citas pendientes por agendar";
    public static final String ERROR_AUTOAGENDADO ="Error al autoagendar turno";

    public static final String SE_NOTIFICO_FARMACIA = "Se ha notificado a farmacia exitosamente";
    public static final String NO_NOTIFICO_FARMACIA = "Se presentó un error al notificar a farmacia";

    public static final String SE_ASIGNO_PROFESIONAL_CITA="Se ha agendado la cita  al profesional exitosamente";
    public static final String NO_ASIGNO_PROFESIONAL_CITA ="Se presentó un error y no se pudo agendar la cita";
    public static final String ERROR_FECHA_CITA="Cita no se puede agendar, la fecha ya esta ocupada";
    public static final String ERROR_FECHA_CITA_HORARIO="Cita no se puede agendar, la fecha esta por fuera del horario del turno";

    public static final String SE_CANCELO_CITA = "La  cita fue cancelada de manera exitosa";
    public static final String ERROR_CANCELAR_CITA = "No se canceló la cita de manera exitosa";
    public static final String CITA_NO_EXISTE ="La cita no existe";
    public static final String SE_DESASIGNO_PROFESIONAL_CITA="Se ha desagendado la cita  al profesional exitosamente";
    public static final String NO_DESASIGNO_PROFESIONAL_CITA="Se presentó un error y no se pudo desagendar la cita";

    public static final String ERROR_ESTADO_CITA="La cita debe estar en estado ";

    public static final String ESTADO_CITA_ACTUALIZADO="Se ha actualizó estado de cita con exito";
    public static final String ERROR_CONFIRMAR_CITA="La cita debe estar agendada";

    public static final String SE_REPROGRAMO_HORA_CITA="Se ha reprogramado hora la hora de la cita exitosamente";
    public static final String NO_REPROGRAMO_HORA_CITA="Se presentó un error y no se pudo reprogramar hora de la cita";

    public static final String SE_ASIGNO_PROFESIONAL_TURNO="Se ha agendado el profesional al turno  ? exitosamente";
    public static final String NO_ASIGNO_PROFESIONAL_TURNO="Se presentó un error y no se pudo asignar el profesional al turno";

    public static final String SE_DESASIGNO_PROFESIONAL_TURNO="Se ha desagendado el profesional al turno  ? exitosamente";
    public static final String NO_DESASIGNO_PROFESIONAL_TURNO="Se presentó un error y no se pudo desasignar el profesional al turno";

    public static final String SE_CALCULA_DESPLAZAMIENTO_CITAS_PROFESIONAL=
            "Se ha actualizó desplazamientos en turno de profesional exitosamente";
    public static final String ERROR_CALCULAR_DESPLAZAMIENTO_CITAS_PROFESIONAL=
            "Se presentó un error y no se pudo actualizar desplazamientos en turno de profesional";

   //profesionales
    public static final String PROFESIONAL_YA_EXISTE="Error al crear profesional, ya existe profesional";
    public static final String PROFESIONAL_NO_EXISTE="Error al actualizar, el profesional no existe profesional";
    public static final String SE_CREA_PROFESIONAL="Se ha creado profesional exitoxamente !!";
    public static final String SE_ACTUALIZA_PROFESIONAL="Se ha actualizado profesional exitoxamente !!";
    public static final String ERROR_CREAR_PROFESIONAL="Error al crear al profesional";
    public static final String ERROR_ACTTUALIZAR_PROFESIONAL="Error al actualizar profesional";

    //conductores
    public static final String CONDUCTOR_YA_EXISTE="Error al crear conductor, ya existe conductor";
    public static final String CONDUCTOR_NO_EXISTE="Error al actualizar, el conductor no existe conductor";
    public static final String SE_CREA_CONDUCTOR="Se ha creado conductor exitoxamente !!";
    public static final String SE_ACTUALIZA_CONDUCTOR="Se ha actualizado conductor exitoxamente !!";
    public static final String ERROR_CREAR_CONDUCTOR="Error al crear al conductor";
    public static final String ERROR_ACTUALIZAR_CONDUCTOR="Error al actualizar conductor";

    //moviles
    public static final String MOVIL_YA_EXISTE="Error al crear movil, ya existe movil";
    public static final String MOVIL_NO_EXISTE="Error al actualizar, el movil no existe movil";
    public static final String SE_CREA_MOVIL="Se ha creado movil exitoxamente !!";
    public static final String SE_ACTUALIZA_MOVIL="Se ha actualizado movil exitoxamente !!";
    public static final String ERROR_CREAR_MOVIL="Error al crear al movil";
    public static final String ERROR_ACTUALIZAR_MOVIL="Error al actualizar movil";

    //turnos profesional
    public static final String SE_ACTUALIZA_TURNO_PROFESIONAL="Se ha actualizado el  turno del profesional";
    public static final String ERROR_ACTUALIZAR_TURNO_PROFESIONAL="Error al actualizar el turno del profesional";

    public static final String RESPUESTA_TURNO = "El profesional tiene citas asignadas";


   private  Mensajes() {
        throw new IllegalStateException("Utility class");
    }
}

