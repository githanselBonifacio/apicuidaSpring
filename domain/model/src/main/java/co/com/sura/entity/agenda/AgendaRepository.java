package co.com.sura.entity.agenda;

import co.com.sura.entity.moviles.Desplazamiento;
import co.com.sura.entity.personal.TurnoProfesional;
import co.com.sura.entity.personal.Profesional;
import co.com.sura.entity.remision.Procedimientos;
import co.com.sura.entity.remision.Tratamiento;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.time.LocalDate;
import java.time.LocalDateTime;

public interface AgendaRepository {
    //profesionales
    Flux<Profesional> consultarProfesionalByTurnoRegional(LocalDate fechaTurno, String idRegional);

    Flux<Profesional> consultarProfesionalFromTurnoRegional(
                            LocalDate fechaTurno, String idCiudad, Integer idHorarioTurno);

    Mono<Boolean>       asignarProfesionalTurno (TurnoProfesional turnoProfesional);
    Mono<Boolean>       desasignarProfesionalTurno (TurnoProfesional turnoProfesional);

    Flux<Profesional> consultarProfesionalesByIdRegional(String idRegional);
    Flux<Actividad>     consultarActividadesByProfesionalesCiudadHorarioTurno(
                            LocalDate fechaTurno,
                            Integer idHorarioTurno,
                            String idRegional
                         );

    //citas
    Flux<Cita> consultarCitasByTurnoRegional(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad);
    Mono<Boolean>        reprogramarCita(LocalDateTime fechaProgramada,
                                         String idCita,
                                         String idProfesional,
                                         LocalDate fechaTurno,
                                         Integer idHorarioTurno,
                                         String idRegional);

    Mono<Boolean>        agendarToProfesional( String idCita,
                                               String idProfesional,
                                               LocalDate fechaTurno,
                                               Integer idHorarioTurno,
                                               String idRegional);

    Mono<Boolean>         desagendarToProfesional(
                                                String idCita,
                                                String idProfesional,
                                                LocalDate fechaTurno,
                                                Integer idHorarioTurno,
                                                String idRegional);

    Mono<Boolean>         desagendarTurnoCompleto(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad);
    Mono<Boolean>         autoagendarTurnoCompleto(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad);

    //desplazamientos
    Flux<Desplazamiento>  consultarDesplazamientoByCitaPartida(
                                        LocalDate fechaProgramada,
                                        Integer idHorarioTurno,
                                        String idCiudad);

    Mono<Boolean>         calcularDesplazamientoCitaByProfesional(
                                        LocalDate fechaProgramada,
                                        Integer idHorarioTurno,
                                        String idCiudad,
                                        String idProfesional);
    //tratamientos
    Flux<Tratamiento>       consultarTratamientoByCitas(String idCita);

    //procedimientos
    Mono<Procedimientos>    consultarProcedimientosByIdCita(String idCita);
}
