package co.com.sura.postgres.repository.agenda.data;


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


    @Query("SELECT cita.* FROM cita " +
            "INNER JOIN turno_cita ON cita.id_cita=turno_cita.id_cita " +
            "WHERE turno_cita.fecha_turno = $1 " +
            "AND turno_cita.id_horario_turno = $2 " +
            "AND cita.id_ciudad = $3 " +
            "AND cita.id_estado != 5;")
    Flux<CitaData> findCitasByTurnoCiudad(
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idCiudad );

    @Query("SELECT cita.* FROM cita " +
            "INNER JOIN turno_cita ON cita.id_cita=turno_cita.id_cita " +
            "WHERE turno_cita.fecha_turno = $1 " +
            "AND turno_cita.id_horario_turno = $2 " +
            "AND cita.id_ciudad = $3 " +
            "AND cita.id_estado != 5 " +
            "AND cita.id_profesional = $4 " +
            "ORDER BY public.cita.fecha_programada ASC;")
    Flux<CitaData> findCitasByTurnoCiudadProfesional(
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idCiudad,
            String numeroIdentificacionProfesional);


    @Query("UPDATE public.cita " +
            "SET  id_estado=2, id_profesional=$2 " +
            "WHERE id_cita = $1 " +
            "AND " +
            "(SELECT id_ciudad from public.profesionales WHERE numero_identificacion = $2) = public.cita.id_ciudad;")
    Mono<Void> agendarToProfesional(String idCita, String idProfesional);


    @Query("UPDATE public.cita " +
            "SET  id_estado=1, id_profesional=NULL " +
            "WHERE id_cita = $1;")
    Mono<Void> desagendarToProfesional(String idCita);


    @Query("UPDATE public.cita " +
            "SET  fecha_programada= $1 " +
            "WHERE id_cita = $2;")
    Mono<Void> actualizarFechaProgramada(LocalDateTime fechaTurno, String idCita);


    @Query("UPDATE cita " +
            "SET id_estado = 1 , id_profesional=NULL " +
            "FROM turno_cita " +
            "WHERE cita.id_cita = turno_cita.id_cita " +
            "AND turno_cita.fecha_turno = $1 " +
            "AND turno_cita.id_horario_turno = $2 " +
            "AND cita.id_ciudad = $3;")
    Mono<Void> desagendarTurnoCompleto(LocalDate fechaTurno, Integer idHorarioTurno,String idCiudad);

    @Query("UPDATE cita " +
            "SET id_estado = 1 , id_profesional=NULL " +
            "FROM turno_cita " +
            "WHERE cita.id_profesional = $3 " +
            "AND turno_cita.fecha_turno = $1 " +
            "AND turno_cita.id_horario_turno = $2; ")
    Mono<Void> desagendarAllFromIdProfesional(
            LocalDate fechaTurno, Integer idHorarioTurno, String idProfesional);

    @Query("INSERT INTO cita " +
            "(id_cita, id_remision, duracion, holgura, fecha_inicio, especialidad, id_ciudad,fecha_programada" +
            ",longitud, latitud)" +
            " VALUES ($1, $2, $3, $4, $5, $6, $7, $8,$9, $10);" )
    Mono<Void> insertCitaQuery(
            @Param("$1") String idCita,
            @Param("$2") String idRemision,
            @Param("$3") Integer duracion,
            @Param("$4") Integer holgura,
            @Param("$5") LocalDateTime fechaInicio,
            @Param("$6") String especialidad,
            @Param("$7") String idCiudad,
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
          citaData.getIdCiudad(),
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
}
