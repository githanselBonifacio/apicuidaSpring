package co.com.sura.postgres.repository.agenda.data;

import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;

public interface DesplazamientoRepository extends ReactiveCrudRepository<DesplazamientoData,Integer> {

    @Query("SELECT * " +
            "FROM public.desplazamiento " +
            "INNER JOIN public.cita ON cita.id_cita = public.desplazamiento.id_cita_partida " +
            "WHERE public.desplazamiento.fecha_programada::date = $1 " +
            "AND public.desplazamiento.id_horario_turno = $2 " +
            "AND public.cita.id_ciudad = $3;")
    Flux<DesplazamientoData> findByIdCitaPartida(
            LocalDate fechaProgramada, Integer idHorarioTurno,String idCiudad);


    @Query("SELECT * " +
            "FROM public.desplazamiento " +
            "INNER JOIN public.cita ON cita.id_cita = public.desplazamiento.id_cita_partida " +
            "AND public.cita.id_profesional = $4" +
            "WHERE public.desplazamiento.fecha_programada::date = $1 " +
            "AND public.desplazamiento.id_horario_turno = $2 " +
            "AND public.cita.id_ciudad = $3;")
    Flux<DesplazamientoData> findByIdCitaPartidaByProfesional(
            LocalDate fechaProgramada, Integer idHorarioTurno,String idCiudad, String idProfesional);

    @Query("DELETE FROM public.desplazamiento " +
            "WHERE public.desplazamiento.id_horario_turno = $2 " +
            "AND public.desplazamiento.fecha_programada::Date = $1 " +
            "AND (id_cita_partida IN ( SELECT id_cita FROM cita WHERE id_profesional = $3) " +
            "OR id_cita_destino IN ( SELECT id_cita FROM cita WHERE id_profesional = $3));")
    Mono<Void> deleteByFechaTurnoProfesional(LocalDate fechaTurno, Integer idHorarioTurno, String idProfesional);

    @Query("DELETE FROM desplazamiento " +
            "WHERE fecha_programada::date = $1 " +
            "  AND id_horario_turno = $2 " +
            "  AND id_cita_partida IN ( " +
            "    SELECT id_cita " +
            "    FROM cita " +
            "    WHERE id_ciudad = $3 " +
            "  );")
    Mono<Void> deleteAllByFechaTurno(LocalDate fechaTurno, Integer idHorarioTurno,String idCiudad);
}
