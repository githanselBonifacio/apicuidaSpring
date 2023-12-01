package co.com.sura.postgres.repository.personal.repository;

import co.com.sura.postgres.repository.personal.data.ProfesionalData;
import co.com.sura.postgres.repository.personal.data.TurnoProfesionalesData;
import org.springframework.data.r2dbc.repository.Modifying;
import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


import java.time.LocalDate;


public interface TurnoProfesionalesRepository  extends ReactiveCrudRepository<TurnoProfesionalesData,Integer> {

    @Query("DELETE FROM public.turno_profesional WHERE fecha_turno=$1 and id_profesional=$2;")
    Mono<Boolean> eliminarByIdProfesionalFechaTurno(LocalDate fechaTurno, String idProfesional);


    @Query("SELECT COUNT(*) FROM public.turno_profesional " +
            "INNER JOIN  public.profesionales ON public.profesionales.numero_identificacion = " +
            "public.turno_profesional.id_profesional " +
            "AND public.turno_profesional.id_regional = $2 " +
            "AND public.turno_profesional.id_horario_turno = $3 " +
            "WHERE fecha_turno = $1 ")
    Mono<Integer> countByFechaTurno(LocalDate fechaTurno , String idRegional, Integer idHorarioTurno);

    @Query("SELECT public.profesionales.* " +
            "FROM public.turno_profesional " +
            "INNER JOIN public.profesionales " +
            "ON public.turno_profesional.id_profesional = public.profesionales.numero_identificacion " +
            "WHERE public.turno_profesional.fecha_turno = $1 " +
            "AND public.turno_profesional.id_horario_turno = $2 " +
            "AND public.profesionales.activo= true " +
            "AND public.profesionales.id_regional= $3;")
    Flux<ProfesionalData> findTurnoProfesionalByCiudadHorario(
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idRegional
    );
    @Modifying
    @Query("DELETE FROM public.turno_profesional " +
            "WHERE fecha_turno=$1 " +
            "AND id_horario_turno= $2 " +
            "AND id_profesional=$3;")
    Mono<Void> deleteByFechaTurnoIdHorarioProfesional(
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idRegional
    );
    @Query("SELECT *" +
            "FROM public.turno_profesional " +
            "WHERE  public.turno_profesional.id_regional=$3 " +
            "AND public.turno_profesional.id_profesional = $2 "+
            "AND to_char( public.turno_profesional.fecha_turno,'YYYY-MM')= $1;")
    Flux<TurnoProfesionalesData> findTurnoProfesionalByFechaRegional(
            String fechaTurno, String idProfesional, String idRegional);
}
