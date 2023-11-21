package co.com.sura.postgres.repository.remision.data;


import co.com.sura.entity.agenda.PacienteTratamientoCita;
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

    @Override
    Mono<Boolean> existsById(String numeroIdentificacion);

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
            "INNER JOIN turno_cita ON turno_cita.id_cita = citas.id_cita " +
            "and turno_cita.fecha_turno = $1 and turno_cita.id_horario_turno= $2 "+
            "INNER JOIN tratamientos ON tratamientos.id_cita = citas.id_cita and citas.id_regional = $3 "+
            "WHERE citas.id_estado = 3 or citas.id_estado = 4 or citas.id_estado = 6 ;")
    Flux<PacienteTratamientoCita> findAllTratamientosPacientesByTurnoRegionalHorario(
            LocalDate turno,Integer idHorario, String idRegional);


    @Query("SELECT pacientes.numero_identificacion,pacientes.tipo_identificacion, " +
            "pacientes.nombres,pacientes.apellidos, " +
            " remisiones.id_remision, " +
            " soporte_nutricionales.* , " +
            " citas.fecha_programada " +
            " FROM pacientes" +
            " INNER JOIN remisiones ON remisiones.numero_identificacion_paciente = pacientes.numero_identificacion " +
            " INNER JOIN citas ON citas.id_remision = remisiones.id_remision " +
            " INNER JOIN turno_cita ON turno_cita.id_cita = citas.id_cita " +
            " and turno_cita.fecha_turno = $1 and turno_cita.id_horario_turno= $2 "+
            " INNER JOIN tratamientos ON tratamientos.id_cita = citas.id_cita and citas.id_regional = $3 "+
            " INNER JOIN soporte_nutricionales ON soporte_nutricionales.id_cita = citas.id_cita " +
            " WHERE citas.id_estado = 3 or citas.id_estado = 4 or citas.id_estado = 6 ;")
    Flux<PacienteTratamientoCita> findAllSoporteNutricionalPacientesByTurnoRegionalHorario(
            LocalDate turno,Integer idHorario, String idRegional
    );
}
