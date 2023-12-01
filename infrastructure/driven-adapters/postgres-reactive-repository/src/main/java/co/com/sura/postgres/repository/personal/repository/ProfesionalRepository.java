package co.com.sura.postgres.repository.personal.repository;


import co.com.sura.postgres.repository.personal.data.ProfesionalData;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;

public interface ProfesionalRepository extends ReactiveCrudRepository<ProfesionalData,String> {

    @Query("SELECT * " +
            "FROM public.profesionales " +
            "Where id_regional = $1;")
    Flux<ProfesionalData> findByIdRegional(@Param("$1") String idRegional);

    @Query("SELECT DISTINCT  public.profesionales.* " +
            "FROM public.profesionales " +
            "LEFT JOIN public.turno_profesional " +
            "on public.profesionales.numero_identificacion = public.turno_profesional.id_profesional " +
            "AND public.turno_profesional.fecha_turno = $1 "+
            "WHERE public.profesionales.id_regional = $2 " +
            "AND public.profesionales.activo = true " +
            "AND public.turno_profesional.id IS NULL;")
    Flux<ProfesionalData> findByTurnoRegional(
            @Param("$1") LocalDate fechaTurno,
            @Param("$2") String idRegional
    );
    @Query("SELECT * " +
            "FROM public.profesionales " +
            "INNER JOIN public.turno_profesional " +
            "on public.profesionales.numero_identificacion = public.turno_profesional.id_profesional " +
            "WHERE public.profesionales.id_regional = $2 " +
            "AND public.profesionales.activo = true " +
            "AND public.turno_profesional.id_horario_turno = $3" +
            "AND public.turno_profesional.fecha_turno = $1")
    Flux<ProfesionalData> findFromTurnoRegional(
            @Param("$1") LocalDate fechaTurno,
            @Param("$2") String idRegional,
            @Param("$3") Integer idHorarioTurno
    );

    @Query("INSERT INTO public.profesionales (numero_identificacion) VALUES ($1);")
    Mono<Void> insertProfesional(@Param("$1") String numeroIdentificacion);


}
