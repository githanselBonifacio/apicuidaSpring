package co.com.sura.postgres.repository.agenda.data;


import co.com.sura.entity.remision.Cita;
import org.springframework.data.r2dbc.repository.Query;

import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import org.springframework.transaction.annotation.Transactional;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

public interface CitaRepository extends ReactiveCrudRepository<CitaData,String> {

    @Query("SELECT EXISTS(SELECT * FROM citas WHERE id_remision = $1 AND id_estado IN (3, 4));")
    Mono<Boolean>validarEstadosCitasToEgreso(String idRemision);

    @Query("UPDATE citas " +
            "SET id_estado = 5 " +
            "WHERE id_remision = $1 " +
            "AND id_estado NOT IN (3, 4, 6) "+
            "AND fecha_programada > NOW();")
    Mono<Boolean> cancelarCitasForEgresoRemision(String idRemision);

    @Query("SELECT citas.*, " +
            "remisiones.numero_identificacion_paciente," +
            "CONCAT (pacientes.nombres,' ',pacientes.apellidos) as paciente," +
            "pacientes.tipo_identificacion as tipo_identificacion_paciente FROM citas " +
            "INNER JOIN turno_cita ON citas.id_cita=turno_cita.id_cita  and turno_cita.fecha_turno = $1" +
            "INNER JOIN remisiones ON citas.id_remision = remisiones.id_remision " +
            "INNER JOIN pacientes ON pacientes.numero_identificacion = remisiones.numero_identificacion_paciente  " +
            "WHERE turno_cita.id_horario_turno = $2 " +
            "AND citas.id_regional = $3 " +
            "AND citas.id_estado != 5;")
    Flux<Cita> findCitasByTurnoCiudad(
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idRegional );

    @Query("SELECT citas.* FROM citas " +
            "INNER JOIN turno_cita ON citas.id_cita=turno_cita.id_cita " +
            "WHERE turno_cita.fecha_turno = $1 " +
            "AND turno_cita.id_horario_turno = $2 " +
            "AND citas.id_regional = $3 " +
            "AND citas.id_estado != 5 " +
            "AND citas.id_profesional = $4 " +
            "ORDER BY public.citas.fecha_programada ASC;")
    Flux<CitaData> findCitasByTurnoCiudadProfesional(
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idRegional,
            String numeroIdentificacionProfesional);


    @Query("UPDATE public.citas " +
            "SET  id_estado=2, id_profesional=$2 " +
            "WHERE id_cita = $1 " +
            "AND " +
            "(SELECT id_regional from public.profesionales WHERE numero_identificacion= $2)= public.citas.id_regional;")
    Mono<Void> agendarToProfesional(String idCita, String idProfesional);


    @Query("UPDATE public.citas " +
            "SET  id_estado=1, id_profesional=NULL " +
            "WHERE id_cita = $1;")
    Mono<Void> desagendarToProfesional(String idCita);


    @Query("UPDATE public.citas " +
            "SET  fecha_programada= $1 " +
            "WHERE id_cita = $2;")
    Mono<Void> actualizarFechaProgramada(LocalDateTime fechaTurno, String idCita);


    @Query("UPDATE citas " +
            "SET id_estado = 1 , id_profesional=NULL " +
            "FROM turno_cita " +
            "WHERE citas.id_cita = turno_cita.id_cita " +
            "AND turno_cita.fecha_turno = $1 " +
            "AND turno_cita.id_horario_turno = $2 " +
            "AND citas.id_regional = $3;")
    Mono<Void> desagendarTurnoCompleto(LocalDate fechaTurno, Integer idHorarioTurno,String idRegional);

    @Query("UPDATE citas " +
            "SET id_estado = 1 , id_profesional=NULL " +
            "FROM turno_cita " +
            "WHERE citas.id_profesional = $3 " +
            "AND turno_cita.fecha_turno = $1 " +
            "AND turno_cita.id_horario_turno = $2; ")
    Mono<Void> desagendarAllFromIdProfesional(
            LocalDate fechaTurno, Integer idHorarioTurno, String idProfesional);

    @Query("INSERT INTO citas " +
            "(id_cita, id_remision, duracion, holgura, fecha_inicio, especialidad, id_regional,fecha_programada" +
            ",longitud, latitud)" +
            " VALUES ($1, $2, $3, $4, $5, $6, $7, $8,$9, $10);" )
    Mono<Void> insertCitaQuery(
            @Param("$1") String idCita,
            @Param("$2") String idRemision,
            @Param("$3") Integer duracion,
            @Param("$4") Integer holgura,
            @Param("$5") LocalDateTime fechaInicio,
            @Param("$6") String especialidad,
            @Param("$7") String idRegional,
            @Param("$8") LocalDateTime fechaProgramada,
            @Param("$9") Double longitud,
            @Param("$10") Double latitud
        );

    default Mono<Void> insertCita(CitaData citaData){
        return  insertCitaQuery(
          citaData.getIdCita(),
          citaData.getIdRemision(),
          citaData.getDuracion(),
          citaData.getHolgura(),
          citaData.getFechaInicio(),
          citaData.getEspecialidad(),
          citaData.getIdRegional(),
          citaData.getFechaInicio(),
          citaData.getLongitud(),
          citaData.getLatitud()
        );
    }

    @Transactional
    default Mono<Void> insertMultiplescitas(List<CitaData> citasData) {
        Flux<CitaData> citasDataFlux = Flux.fromIterable(citasData);

        return citasDataFlux
                .flatMap(this::insertCita)
                .then();
    }
    @Query("SELECT get_last_consecutive_number_cita($1);")
    Mono<Integer> findLastNumberCitaRemision(String idRemision);


    @Query("CALL public.delete_cita_data(:idRemision,:fechaAplicacionNovedad)")
    Mono<Void> deleteCitaDataByIdRemision(
            @Param("idRemision") String idRemision,
            @Param("fechaAplicacionNovedad") LocalDateTime fechaAplicacionNovedad);
}
