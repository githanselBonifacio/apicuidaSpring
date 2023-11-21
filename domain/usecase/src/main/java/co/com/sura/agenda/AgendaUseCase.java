package co.com.sura.agenda;

import co.com.sura.entity.agenda.Cita;
import co.com.sura.entity.personal.TurnoProfesional;
import co.com.sura.entity.remision.Procedimientos;
import co.com.sura.entity.remision.Tratamiento;
import co.com.sura.entity.agenda.Actividad;
import co.com.sura.entity.agenda.AgendaRepository;
import co.com.sura.entity.personal.Profesional;
import co.com.sura.entity.moviles.Desplazamiento;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.time.LocalDateTime;

public class AgendaUseCase  {

    private final AgendaRepository agendaRepository;

    public AgendaUseCase(AgendaRepository agendaRepository) {
        this.agendaRepository = agendaRepository;
    }


    public Flux<Profesional> consultarProfesionalesByTurnoRegional(LocalDate fechaTurno, String idRegional) {
        return agendaRepository.consultarProfesionalByTurnoRegional(fechaTurno,idRegional);
    }

    public Flux<Profesional> consultarProfesionalesFromTurnoRegional(
            LocalDate fechaTurno, String idCiudad, Integer idHorarioTurno) {
        return agendaRepository.consultarProfesionalFromTurnoRegional(fechaTurno,idCiudad,idHorarioTurno);
    }

    public Mono<Boolean> asignarProfesionalTurno(TurnoProfesional turnoProfesional){
        return agendaRepository.asignarProfesionalTurno(turnoProfesional);
    }

    public Mono<Boolean> desasignarProfesionalTurno(TurnoProfesional turnoProfesional){
        return agendaRepository.desasignarProfesionalTurno(turnoProfesional);
    }

    public Mono<Boolean> desagendarTurnoCompleto(LocalDate fechaTurno,Integer idHorarioTurno,String idRegional){
        return agendaRepository.desagendarTurnoCompleto(fechaTurno,idHorarioTurno,idRegional);
    }

    public Flux<Profesional> consultarProfesionalesByRegional(String idRegional) {
        return agendaRepository.consultarProfesionalesByIdRegional(idRegional);
    }

    public Flux<Actividad> consultarActividadesProfesionalesRegionalHorario(
          LocalDate fechaTurno,
          Integer idHorarioTurno,
          String idRegional) {
        return agendaRepository
                .consultarActividadesByProfesionalesCiudadHorarioTurno(fechaTurno,idHorarioTurno,idRegional);
    }
     public Mono<Boolean> autoagendarTurnoCompleto( LocalDate fechaTurno,
                                                 Integer idHorarioTurno,
                                                 String idRegional){
        return agendaRepository.autoagendarTurnoCompleto(fechaTurno,idHorarioTurno,idRegional);
     }

    public Flux<Desplazamiento> consultarDesplazamientoByIdCitaPartida(
            LocalDate fechaProgramada, Integer idHorarioTurno,String idCiudad){
        return agendaRepository.consultarDesplazamientoByCitaPartida(
                fechaProgramada,idHorarioTurno,idCiudad);
    }


    public Flux<Cita> consultarCitasByTurnoRegional(LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad){
        return agendaRepository.consultarCitasByTurnoRegional(fechaTurno, idHorarioTurno, idCiudad);
    }
    public Mono<Boolean> reprogramarCitaById (
            LocalDateTime fechaProgramada,
            String idCita,
            String idProfesional,
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idRegional){
        return agendaRepository.reprogramarCita(
                fechaProgramada, idCita,idProfesional,fechaTurno,idHorarioTurno,idRegional);
    }

    public Mono<Boolean> asignarProfesionaCita (
            String idCita,
            String idProfesional,
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idRegional){
        return agendaRepository.agendarToProfesional( idCita,idProfesional,fechaTurno,idHorarioTurno,idRegional);
    }

    public Mono<Boolean> desasignarProfesionaCita (
            String idCita,String idProfesional,LocalDate fechaTurno,Integer idHorarioTurno,String idRegional){
        return agendaRepository.desagendarToProfesional(idCita,idProfesional, fechaTurno,idHorarioTurno,idRegional);
    }
    //tratamientos
    public Flux<Tratamiento> consultarTratamientosByCita(String idCita){
        return agendaRepository.consultarTratamientoByCitas(idCita);
    }

    //procedimientos
    public Mono<Procedimientos> consultarProcedimietosByIdCita(String idCita){
        return agendaRepository.consultarProcedimientosByIdCita(idCita);
    }

}
