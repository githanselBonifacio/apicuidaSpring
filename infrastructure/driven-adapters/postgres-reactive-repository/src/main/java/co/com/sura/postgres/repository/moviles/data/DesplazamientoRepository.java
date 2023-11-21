package co.com.sura.postgres.repository.moviles.data;

import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;

public interface DesplazamientoRepository extends ReactiveCrudRepository<DesplazamientoData,Integer> {

    @Query("SELECT * FROM public.desplazamientos " +
            "INNER JOIN public.citas ON public.citas.id_cita = public.desplazamientos.id_cita_partida AND " +
            "public.citas.id_regional = $2 AND " +
            "public.citas.id_horario_turno = $3 " +
            "WHERE public.desplazamientos.fecha_programada::date = $1")
    Flux<DesplazamientoData> findByFechaProgramada(LocalDate fechaProgramada, String idRegional,Integer idHorarioTurno);

    @Query("SELECT * " +
            "FROM public.desplazamiento " +
            "INNER JOIN public.cita ON cita.id_cita = public.desplazamiento.id_cita_partida " +
            "WHERE public.desplazamiento.fecha_programada::date = $1 " +
            "AND public.desplazamiento.id_horario_turno = $2 " +
            "AND public.cita.id_regional = $3;")
    Flux<DesplazamientoData> findByIdCitaPartida(
            LocalDate fechaProgramada, Integer idHorarioTurno,String idRegional);


    @Query("SELECT * " +
            "FROM public.desplazamientos " +
            "INNER JOIN public.citas ON citas.id_cita = public.desplazamientos.id_cita_partida " +
            "AND public.citas.id_profesional = $4" +
            "WHERE public.desplazamientos.fecha_programada::date = $1 " +
            "AND public.desplazamientos.id_horario_turno = $2 " +
            "AND public.citas.id_regional = $3;")
    Flux<DesplazamientoData> findByIdCitaPartidaByProfesional(
            LocalDate fechaProgramada, Integer idHorarioTurno,String idRegional, String idProfesional);

    @Query("DELETE FROM public.desplazamientos " +
            "WHERE public.desplazamientos.id_horario_turno = $2 " +
            "AND public.desplazamientos.fecha_programada::Date = $1 " +
            "AND (id_cita_partida IN ( SELECT id_cita FROM citas WHERE id_profesional = $3) " +
            "OR id_cita_destino IN ( SELECT id_cita FROM citas WHERE id_profesional = $3));")
    Mono<Void> deleteByFechaTurnoProfesional(LocalDate fechaTurno, Integer idHorarioTurno, String idProfesional);

    @Query("DELETE FROM desplazamientos " +
            "WHERE fecha_programada::date = $1 " +
            "  AND id_horario_turno = $2 " +
            "  AND id_cita_partida IN ( " +
            "    SELECT id_cita " +
            "    FROM citas " +
            "    WHERE id_regional = $3 " +
            "  );")
    Mono<Void> deleteAllByFechaTurno(LocalDate fechaTurno, Integer idHorarioTurno,String idRegional);
}
