package co.com.sura.postgres.repository.agenda.data;

import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;

public interface TurnoProfesionalesRepository  extends ReactiveCrudRepository<TurnoProfesionalesData,String> {

    @Query("SELECT public.profesionales.* " +
            "FROM public.turno_profesional " +
            "INNER JOIN public.profesionales " +
            "ON public.turno_profesional.id_profesional = public.profesionales.numero_identificacion " +
            "WHERE public.turno_profesional.fecha_turno = $1 " +
            "AND public.turno_profesional.id_horario_turno = $2 " +
            "AND public.profesionales.activo= true " +
            "AND public.profesionales.id_ciudad= $3;")
    Flux<ProfesionalData> findTurnoProfesionalByCiudadHorario(
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idCiudad
    );

    @Query("DELETE FROM public.turno_profesional " +
            "WHERE fecha_turno=$1 " +
            "AND id_horario_turno= $2 " +
            "AND id_profesional=$3;")
    Mono<Void> deleteByFechaTurnoIdHorarioProfesional(
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idCiudad
    );
}
