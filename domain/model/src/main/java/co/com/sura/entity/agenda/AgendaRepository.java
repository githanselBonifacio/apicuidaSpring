package co.com.sura.entity.agenda;

import co.com.sura.entity.moviles.Desplazamiento;
import co.com.sura.entity.personal.TurnoProfesional;
import co.com.sura.entity.remision.Procedimientos;
import co.com.sura.entity.remision.Tratamiento;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.time.LocalDate;
import java.time.LocalDateTime;

public interface AgendaRepository {
    //profesionales

    Mono<Boolean>       asignarProfesionalTurno (TurnoProfesional turnoProfesional);
    Mono<Boolean>       desasignarProfesionalTurno (TurnoProfesional turnoProfesional);


    Flux<Actividad>     consultarActividadesByProfesionalesCiudadHorarioTurno(
                            LocalDate fechaTurno,
                            Integer idHorarioTurno,
                            String idRegional
                         );

    //citas
    Flux<Cita>           consultarCitasByTurnoRegional(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad);
    Mono<Boolean>        reprogramarCitaFromProfesional(
                                         LocalDateTime fechaProgramada,
                                         String idCita,
                                         String idProfesional,
                                         LocalDate fechaTurno,
                                         Integer idHorarioTurno,
                                         String idRegional);



    //desplazamientos
    Flux<Desplazamiento>  consultarDesplazamientoByCitaPartida(
                                        LocalDate fechaProgramada,
                                        Integer idHorarioTurno,
                                        String idCiudad);

    //tratamientos
    Flux<Tratamiento>       consultarTratamientoByCitas(String idCita);

    //procedimientos
    Mono<Procedimientos>    consultarProcedimientosByIdCita(String idCita);
}
