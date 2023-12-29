package co.com.sura.postgres.agenda.repository;


import co.com.sura.agenda.entity.Cita;
import co.com.sura.postgres.agenda.data.CitaData;
import org.springframework.data.r2dbc.repository.Modifying;
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

    @Query("SELECT EXISTS(SELECT * FROM citas WHERE id_remision = $1 AND id_estado IN ($2, $3));")
    Mono<Boolean> validarEstadosToEgreso(String idRemision, Integer idEstadoConfirmada, Integer idEstadoProgreso);


    @Query("SELECT * FROM citas WHERE fecha_programada::Date = $1 AND id_regional=$2")
    Flux<CitaData> findAllByFechaTurnoRegional(LocalDate fechaTurno, String idRegional);


    @Query("SELECT citas.*, " +
            "remisiones.numero_identificacion_paciente," +
            "CONCAT (pacientes.nombres,' ',pacientes.apellidos) as paciente," +
            "pacientes.tipo_identificacion as tipo_identificacion_paciente FROM citas " +
            "INNER JOIN remisiones ON citas.id_remision = remisiones.id_remision " +
            "INNER JOIN pacientes ON pacientes.numero_identificacion = remisiones.numero_identificacion_paciente  " +
            "WHERE citas.fecha_programada::Date = $1" +
            "AND citas.id_horario_turno = $2 " +
            "AND citas.id_regional = $3 " +
            "AND citas.id_estado != $4;")
    Flux<Cita> findAllByTurnoRegionalHorario(
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idRegional,
            Integer idEstadoCancelado);

    @Query("SELECT citas.* FROM citas " +
            "WHERE citas.fecha_programada::Date = $1 " +
            "AND citas.id_horario_turno = $2 " +
            "AND citas.id_regional = $3 " +
            "AND citas.id_estado != $5 " +
            "AND citas.id_profesional = $4 " +
            "ORDER BY public.citas.fecha_programada ASC;")
    Flux<CitaData> findAllByTurnoRegionalProfesional(
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idRegional,
            String numeroIdentificacionProfesional,
            Integer idEstadoCancelado);

    @Query("SELECT citas.* FROM citas " +
            "WHERE fecha_programada::Date = $1 " +
            "AND citas.id_profesional = $2 " +
            "ORDER BY citas.fecha_programada ASC;")
    Flux<CitaData> findAllByTurnoProfesional(
            LocalDate fechaTurno,
            String numeroIdentificacionProfesional);

    @Query("SELECT citas.* FROM citas WHERE id_cita LIKE concat('%-', $1)")
    Mono<CitaData> findSedeByIdRegional(String idRegional);

    @Query("SELECT citas.* FROM citas WHERE id_remision = $1")
    Flux<CitaData> findAllByIdRemision(String idRemision);

    //gestion de estados
    @Modifying
    @Query("UPDATE citas SET id_estado = $2 " +
            "WHERE id_remision = $1 AND id_estado NOT IN ($3, $4) AND fecha_programada > NOW();")
    Mono<Boolean> cancelarToEgreso(
            String idRemision, Integer estadoCancelado, Integer estadoSinAgendar,Integer estadoAgendada);


    @Modifying
    @Query("UPDATE citas SET  id_estado=$2, id_profesional=$3 WHERE citas.id_cita = $1;")
    Mono<Void> updateEstadoAndProfesional(String idCita, Integer idEstado, String idProfesional);

    @Modifying
    @Query("UPDATE citas SET  id_estado=$2 WHERE citas.id_cita = $1;")
    Mono<Void> updateEstado(String idCita, Integer idEstado);

    @Modifying
    @Query("UPDATE citas SET  fecha_programada= $1 WHERE id_cita = $2;")
    Mono<Void> updateFechaProgramada(LocalDateTime fechaTurno, String idCita);


    @Modifying
    @Query("UPDATE citas " +
            "SET id_estado = $4 , id_profesional=NULL " +
            "WHERE citas.fecha_programada::Date = $1  " +
            "AND citas.id_horario_turno = $2 " +
            "AND citas.id_regional = $3;")
    Mono<Void> desagendarTurnoCompleto(
            LocalDate fechaTurno, Integer idHorarioTurno,String idRegional,Integer estadoNoAgendado);

    @Modifying
    @Query("UPDATE citas " +
            "SET id_estado = $4 , id_profesional=NULL " +
            "WHERE citas.id_profesional = $3 " +
            "AND citas.fecha_programada::Date = $1 " +
            "AND citas.id_horario_turno = $2; ")
    Mono<Void> desagendarAllFromIdProfesional(
            LocalDate fechaTurno, Integer idHorarioTurno, String idProfesional,Integer estadoNoAgendado);


    //consultar rango de fechas
    @Query("SELECT * FROM citas " +
            "WHERE fecha_programada::Date = $1::Date AND " +
            " id_cita != $2 AND id_horario_turno= $3 AND id_regional= $4 AND id_profesional = $5 AND " +
            " fecha_programada < $1 " +
            "ORDER BY (fecha_programada - $1::timestamp) DESC " +
            "LIMIT 1; ")
    Mono<CitaData> findMasCercanaAnterior(LocalDateTime fechaCita, String idCita,
                                          Integer idHorarioTurno, String idRegional, String idProfesional);

    @Query("SELECT * FROM citas " +
            "WHERE fecha_programada::Date = $1::Date AND " +
            " id_cita != $2 AND id_horario_turno= $3 AND id_regional= $4 AND id_profesional = $5 AND " +
            " fecha_programada > $1 " +
            "ORDER BY (fecha_programada - $1::timestamp) ASC " +
            "LIMIT 1; ")
    Mono<CitaData> findMasCercanaPosterior(LocalDateTime fechaCita, String idCita,
                                           Integer idHorarioTurno, String idRegional, String idProfesional);

    //insertar registros
    @Query("INSERT INTO citas (id_cita,id_remision) VALUES ($1,$2);" )
    Mono<Void> insertCitaQuery(@Param("$1") String idCita,@Param("$2") String idRemision);

    private Mono<Void> insertCita(CitaData citaData){
        return  insertCitaQuery(citaData.getIdCita(), citaData.getIdRemision());
    }

    @Transactional
    default Mono<Void> insertMultiplescitas(List<CitaData> citasData) {
        Flux<CitaData> citasDataFlux = Flux.fromIterable(citasData);
        return citasDataFlux
                .flatMap(this::insertCita)
                .then();
    }

    @Query(value = "SELECT  MAX(SPLIT_PART(id_cita, '-', 2)::int) as mc FROM citas WHERE id_remision = $1")
    Mono<Integer> findLastNumberIdCita(String idRemision);

    @Query("CALL public.delete_cita_data(:idRemision,:fechaAplicacionNovedad)")
    Mono<Void> deleteCitaDataByIdRemision(
            @Param("idRemision") String idRemision,
            @Param("fechaAplicacionNovedad") LocalDateTime fechaAplicacionNovedad);
}
