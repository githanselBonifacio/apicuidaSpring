package co.com.sura.agenda;

import co.com.sura.agenda.entity.Actividad;
import co.com.sura.agenda.gateway.AgendaRepository;
import co.com.sura.agenda.gateway.AgendamientoAutomaticoRepository;
import co.com.sura.agenda.entity.Cita;
import co.com.sura.agenda.gateway.GestionEstadosCitasRepository;
import co.com.sura.personal.gateway.PersonalCrudRepository;
import co.com.sura.personal.entity.TurnoProfesional;
import co.com.sura.remision.entity.procedimientos.Procedimientos;
import co.com.sura.remision.entity.datosremision.Tratamiento;
import co.com.sura.personal.entity.Profesional;
import co.com.sura.moviles.entity.Desplazamiento;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.time.LocalDateTime;

public class AgendaUseCase  {

    private final AgendaRepository agendaRepository;
    private final PersonalCrudRepository personalCrudRepository;
    private final GestionEstadosCitasRepository gestionEstadosCitasRepository;
    private final AgendamientoAutomaticoRepository agendamientoAutomaticoRepository;

    public AgendaUseCase(AgendaRepository agendaRepository, PersonalCrudRepository personalCrudRepository,
                         GestionEstadosCitasRepository gestionEstadosCitasRepository,
                         AgendamientoAutomaticoRepository agendamientoAutomaticoRepository) {
        this.agendaRepository = agendaRepository;
        this.personalCrudRepository = personalCrudRepository;
        this.gestionEstadosCitasRepository=gestionEstadosCitasRepository;
        this.agendamientoAutomaticoRepository = agendamientoAutomaticoRepository;
    }


    public Flux<Profesional> consultarProfesionalesByTurnoRegional(LocalDate fechaTurno, String idRegional) {
        return personalCrudRepository.consultarProfesionalByTurnoRegional(fechaTurno,idRegional);
    }

    public Flux<Profesional> consultarProfesionalesFromTurnoRegional(
            LocalDate fechaTurno, String idRegional, Integer idHorarioTurno) {
        return personalCrudRepository.consultarProfesionalFromTurnoRegional(fechaTurno,idRegional,idHorarioTurno);
    }

    public Flux<Profesional> consultarProfesionalesByRegional(String idRegional) {
        return personalCrudRepository.consultarProfesionalesByIdRegional(idRegional);
    }

    public Mono<Boolean> asignarProfesionalTurno(TurnoProfesional turnoProfesional){
        return agendaRepository.asignarProfesionalTurno(turnoProfesional);
    }

    public Mono<Boolean> desasignarProfesionalTurno(TurnoProfesional turnoProfesional){
        return agendaRepository.desasignarProfesionalTurno(turnoProfesional);
    }

    public Mono<Boolean> desagendarTurnoCompleto(LocalDate fechaTurno,Integer idHorarioTurno,String idRegional){
        return agendamientoAutomaticoRepository.desagendarTurnoCompleto(fechaTurno,idHorarioTurno,idRegional);
    }



    public Flux<Actividad> consultarActividadesProfesionalesRegionalHorario(
          LocalDate fechaTurno,
          Integer idHorarioTurno,
          String idRegional) {
        return agendaRepository
                .consultarActividadesByProfesionalesRegionalHorarioTurno(fechaTurno,idHorarioTurno,idRegional);
    }
     public Mono<Boolean> autoagendarTurnoCompleto( LocalDate fechaTurno,
                                                 Integer idHorarioTurno,
                                                 String idRegional){
        return agendamientoAutomaticoRepository.autoagendarTurnoCompleto(fechaTurno,idHorarioTurno,idRegional);
     }

    public Flux<Desplazamiento> consultarDesplazamientoByTurnoRegional(
            LocalDate fechaProgramada, Integer idHorarioTurno,String idRegional){
        return agendaRepository.consultarDesplazamientoRegional(
                fechaProgramada,idHorarioTurno,idRegional);
    }


    public Flux<Cita> consultarCitasByTurnoRegional(LocalDate fechaTurno, Integer idHorarioTurno, String idRegional){
        return agendaRepository.consultarCitasByTurnoRegional(fechaTurno, idHorarioTurno, idRegional);
    }
    public Mono<Boolean> reprogramarCitaById (
            LocalDateTime fechaProgramada,
            String idCita,
            String idProfesional,
            LocalDate fechaTurno,
            Integer idHorarioTurno,
            String idRegional){
        return agendaRepository.reprogramarCitaFromProfesional(
                fechaProgramada, idCita,idProfesional,fechaTurno,idHorarioTurno,idRegional);
    }

    public Mono<Boolean> asignarProfesionaCita (
            String idCita,
            String idProfesional,
            LocalDateTime fechaProgramada,
            Integer idHorarioTurno,
            String idRegional){
        return gestionEstadosCitasRepository
                .agendarToProfesional( idCita,idProfesional,fechaProgramada,idHorarioTurno,idRegional)
                .onErrorResume(e->Mono.error(new Exception(e.getMessage())));
    }

    public Mono<Boolean> desasignarProfesionaCita (
            String idCita,String idProfesional,LocalDate fechaTurno,Integer idHorarioTurno,String idRegional){
        return gestionEstadosCitasRepository
                .desagendarToProfesional(idCita,idProfesional, fechaTurno,idHorarioTurno,idRegional);
    }
    public Mono<Boolean> confirmarCita(String idCita){
        return gestionEstadosCitasRepository.confirmarCita(idCita);
    }
    public Mono<Boolean> iniciarAtencionCita(String idCita){
        return gestionEstadosCitasRepository.iniciarAtencionCita(idCita);
    }
    public Mono<Boolean> finalizarAtencionCita(String idCita){
        return gestionEstadosCitasRepository.finalizarAtencionCita(idCita);
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
