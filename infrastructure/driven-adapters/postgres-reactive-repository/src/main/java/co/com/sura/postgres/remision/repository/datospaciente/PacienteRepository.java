package co.com.sura.postgres.remision.repository.datospaciente;


import co.com.sura.agenda.entity.PacienteTratamientoCita;
import co.com.sura.postgres.remision.data.datospaciente.PacienteData;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;


public interface PacienteRepository extends ReactiveCrudRepository<PacienteData,String> {

    @Query("SELECT public.pacientes.*, to_json(public.ubicaciones.*) as id_ubicaciones " +
            "FROM public.remisiones " +
            "INNER JOIN public.pacientes ON public.pacientes.numero_identificacion = " +
            "public.remisiones.numero_identificacion_paciente " +
            "INNER JOIN public.ubicaciones ON public.pacientes.id_ubicacion = public.ubicaciones.id_ubicacion " +
            "WHERE public.remisiones.id_remision = $1;")
    Mono<PacienteData> findPacienteByNumeroIdRemision(String idRemision);

    @Query("INSERT INTO pacientes( " +
            " numero_identificacion) " +
            " VALUES " +
            "($1)"
    )
    Mono<Boolean> insertNuevoPaciente(
            @Param("$1")   String numeroIdentificacion);

    @Query("SELECT pacientes.numero_identificacion, " +
            "pacientes.tipo_identificacion,pacientes.nombres,pacientes.apellidos, " +
            "remisiones.id_remision, "+
            "tratamientos.* , " +
            "citas.fecha_programada " +
            "FROM pacientes " +
            "INNER JOIN remisiones ON remisiones.numero_identificacion_paciente = pacientes.numero_identificacion " +
            "INNER JOIN citas ON citas.id_remision = remisiones.id_remision " +
            "INNER JOIN tratamientos ON tratamientos.id_cita = citas.id_cita "+
            "WHERE citas.id_estado = 3 or citas.id_estado = 4 or citas.id_estado = 6 ;")
    Flux<PacienteTratamientoCita> findAllTratamientosPacientes();


    @Query("SELECT pacientes.numero_identificacion, " +
            "pacientes.tipo_identificacion,pacientes.nombres,pacientes.apellidos, " +
            "remisiones.id_remision, "+
            "soporte_nutricionales.* , " +
            "citas.fecha_programada " +
            "FROM pacientes " +
            "INNER JOIN remisiones ON remisiones.numero_identificacion_paciente = pacientes.numero_identificacion " +
            "INNER JOIN citas ON citas.id_remision = remisiones.id_remision " +
            "INNER JOIN soporte_nutricionales ON soporte_nutricionales.id_cita = citas.id_cita "+
            "WHERE citas.id_estado = 3 or citas.id_estado = 4 or citas.id_estado = 6 ;")
    Flux<PacienteTratamientoCita> findAllSoporteNutricionalPacientes();

    @Query("SELECT pacientes.numero_identificacion,pacientes.tipo_identificacion, " +
            "pacientes.nombres,pacientes.apellidos,  " +
            "remisiones.id_remision, "+
            "tratamientos.* , " +
            "citas.fecha_programada " +
            "FROM pacientes " +
            "INNER JOIN remisiones ON remisiones.numero_identificacion_paciente = pacientes.numero_identificacion " +
            "INNER JOIN citas ON citas.id_remision = remisiones.id_remision " +
            "INNER JOIN tratamientos ON tratamientos.id_cita = citas.id_cita and citas.id_regional = $2 "+
            "WHERE citas.fecha_programada::Date = $1 and citas.id_horario_turno = $3 and citas.id_estado = 3 or " +
            " citas.id_estado = 4 or citas.id_estado = 6 ;")
    Flux<PacienteTratamientoCita> findAllTratamientosPacientesByTurnoRegionalHorario(
            LocalDate fechaTurno, String idRegional, Integer idHorarioTurno);


    @Query("SELECT pacientes.numero_identificacion,pacientes.tipo_identificacion, " +
            "pacientes.nombres,pacientes.apellidos, " +
            " remisiones.id_remision, " +
            " soporte_nutricionales.* , " +
            " citas.fecha_programada " +
            " FROM pacientes" +
            " INNER JOIN remisiones ON remisiones.numero_identificacion_paciente = pacientes.numero_identificacion " +
            " INNER JOIN citas ON citas.id_remision = remisiones.id_remision " +
            " INNER JOIN tratamientos ON tratamientos.id_cita = citas.id_cita and citas.id_regional = $2 "+
            " INNER JOIN soporte_nutricionales ON soporte_nutricionales.id_cita = citas.id_cita " +
            " WHERE citas.fecha_programada::Date = $1 and citas.id_horario_turno = $3 and citas.id_estado = 3 or " +
            " citas.id_estado = 4 or citas.id_estado = 6 ;")
    Flux<PacienteTratamientoCita> findAllSoporteNutricionalPacientesByTurnoRegionalHorario(
            LocalDate fechaTurno, String idRegional, Integer idHorarioTurno);
}
