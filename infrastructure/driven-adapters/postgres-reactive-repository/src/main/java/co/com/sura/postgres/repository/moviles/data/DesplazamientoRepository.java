package co.com.sura.postgres.repository.moviles.data;

import org.springframework.data.r2dbc.repository.Query;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;

public interface DesplazamientoRepository extends ReactiveCrudRepository<DesplazamientoData,Integer> {

    @Query("SELECT * FROM desplazamientos WHERE id_cita_partida = $1;")
    Mono<DesplazamientoData> findByIdCitaPartida(String idCitaPartida);

    @Query("SELECT * FROM public.desplazamientos " +
            "WHERE public.desplazamientos.fecha_programada::date = $1 " +
            "AND public.desplazamientos.id_regional = $2 "+
            "AND public.desplazamientos.id_horario_turno = $3 ")
    Flux<DesplazamientoData> findByFechaProgramada(LocalDate fechaProgramada, String idRegional,Integer idHorarioTurno);


    @Query("SELECT * " +
            "FROM public.desplazamientos " +
            "WHERE public.desplazamientos.fecha_programada::date = $1 " +
            "AND public.desplazamientos.id_horario_turno = $2 " +
            "AND public.desplazamientos.id_regional = $3 " +
            "AND public.desplazamientos.id_profesional = $4;")
    Flux<DesplazamientoData> findAllByTurnoProfesional(
            LocalDate fechaProgramada, Integer idHorarioTurno,String idRegional, String idProfesional);

    @Query("DELETE FROM public.desplazamientos " +
            "WHERE public.desplazamientos.fecha_programada::Date = $1 " +
            "AND public.desplazamientos.id_horario_turno = $2  " +
            "AND public.desplazamientos.id_regional = $3 " +
            "AND public.desplazamientos.id_profesional = $4;")
    Mono<Void> deleteByFechaTurnoProfesional(
            LocalDate fechaProgramada, Integer idHorarioTurno,String idRegional, String idProfesional);

    @Query("DELETE FROM desplazamientos " +
            "WHERE fecha_programada::date = $1 " +
            "AND id_horario_turno = $2 " +
            "AND public.desplazamientos.id_regional = $3;")
    Mono<Void> deleteAllByFechaTurno(LocalDate fechaTurno, Integer idHorarioTurno,String idRegional);
}
