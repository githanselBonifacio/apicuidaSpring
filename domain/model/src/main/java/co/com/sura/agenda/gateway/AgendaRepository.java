package co.com.sura.agenda.gateway;

import co.com.sura.agenda.entity.Actividad;
import co.com.sura.agenda.entity.Cita;
import co.com.sura.moviles.entity.Desplazamiento;
import co.com.sura.personal.entity.TurnoProfesional;
import co.com.sura.remision.entity.procedimientos.Procedimientos;
import co.com.sura.remision.entity.datosremision.Tratamiento;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.time.LocalDate;
import java.time.LocalDateTime;

public interface AgendaRepository {
    //profesionales

    Mono<Boolean>       asignarProfesionalTurno (TurnoProfesional turnoProfesional);
    Mono<Boolean>       desasignarProfesionalTurno (TurnoProfesional turnoProfesional);


    Flux<Actividad> consultarActividadesByProfesionalesRegionalHorarioTurno(
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
    Flux<Desplazamiento> consultarDesplazamientoRegional(
                                        LocalDate fechaProgramada,
                                        Integer idHorarioTurno,
                                        String idRegional);

    //tratamientos
    Flux<Tratamiento>       consultarTratamientoByCitas(String idCita);

    //procedimientos
    Mono<Procedimientos>    consultarProcedimientosByIdCita(String idCita);
}
