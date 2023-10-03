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
            " numero_identificacion," +
            " tipo_identificacion," +
            " nombre," +
            " apellido," +
            " edad," +
            " sexo," +
            " peso," +
            " tipo_afiliacion," +
            " nombre_aseguradora," +
            " fecha_nacimiento," +
            " id_ubicacion) " +
            " VALUES " +
            "($1, $2, $3, $4, $5, $6, $7, $8, $9, $10,$11)"
    )
    Mono<Void> insertPacienteQuery(
            @Param("$1")   String numeroIdentificacion,
            @Param("$2")   String tipoIdentificacion,
            @Param("$3")   String nombre,
            @Param("$4")   String apellido,
            @Param("$5")   String edad,
            @Param("$6")   String sexo,
            @Param("$7")   String peso,
            @Param("$8")   String tipoAfiliacion,
            @Param("$9")   String nombreAseguradora,
            @Param("$10")  LocalDate fechaNacimiento,
            @Param("$11")  String idUbicacion
    );
    default Mono<Void> insertpaciente(PacienteData pacienteData){
        return insertPacienteQuery(
                pacienteData.getNumeroIdentificacion(),
                pacienteData.getTipoIdentificacion(),
                pacienteData.getNombres(),
                pacienteData.getApellidos(),
                pacienteData.getEdad(),
                pacienteData.getSexo(),
                pacienteData.getPeso(),
                pacienteData.getTipoAfiliacion(),
                pacienteData.getNombreAseguradora(),
                pacienteData.getFechaNacimiento(),
                pacienteData.getIdUbicacion()
        );
    }
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
