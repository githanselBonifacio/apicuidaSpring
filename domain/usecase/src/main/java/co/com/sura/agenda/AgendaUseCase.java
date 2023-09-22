package co.com.sura.agenda;

import co.com.sura.entity.agenda.*;
import co.com.sura.entity.moviles.Desplazamiento;
import co.com.sura.entity.remision.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.time.LocalDateTime;

public class AgendaUseCase implements AgendaFactory {

    private final AgendaRepository agendaRepository;

    public AgendaUseCase(AgendaRepository agendaRepository) {
        this.agendaRepository = agendaRepository;
    }


    public Flux<Profesional> consultarProfesionales() {
        return agendaRepository.consultarProfesionales();
    }

    public Flux<Profesional> consultarProfesionalesByTurnoCiudad(LocalDate fechaTurno, String idRegional) {
        return agendaRepository.consultarProfesionalByTurnoCiudad(fechaTurno,idRegional);
    }

    public Flux<Profesional> consultarProfesionalesFromTurnoCiudad(
            LocalDate fechaTurno, String idCiudad, Integer idHorarioTurno) {
        return agendaRepository.consultarProfesionalFromTurnoCiudad(fechaTurno,idCiudad,idHorarioTurno);
    }

    public Mono<Boolean> asignarProfesionalTurno(LocalDate fechaTurno,Integer idHorarioTurno,String idProfesional){
        return agendaRepository.asignarProfesionalTurno(fechaTurno,idHorarioTurno,idProfesional);
    }

    public Mono<Boolean> desasignarProfesionalTurno(LocalDate fechaTurno,Integer idHorarioTurno,String idProfesional){
        return agendaRepository.desasignarProfesionalTurno(fechaTurno,idHorarioTurno,idProfesional);
    }

    public Mono<Boolean> desagendarTurnoCompleto(LocalDate fechaTurno,Integer idHorarioTurno,String idRegional){
        return agendaRepository.desagendarTurnocompleto(fechaTurno,idHorarioTurno,idRegional);
    }

    public Flux<Profesional> consultarProfesionalesByCiudad(String idRegional) {
        return agendaRepository.consultarProfesionalesByIdCiudad(idRegional);
    }

    public Flux<Actividad> consultarActividadesProfesionalesCiudadHorario(
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
    public Mono<Boolean> calcularDesplazamientoCitaByProfesional(
            LocalDate fechaProgramada, Integer idHorarioTurno, String idRegional, String idProfesional){
        return agendaRepository.calcularDesplazamientoCitaByProfesional(
                fechaProgramada,idHorarioTurno,idRegional,idProfesional
        );
    }

    public Mono<Profesional> crearProfesional(Profesional profesional) {
        return agendaRepository.crearProfesional(profesional);
    }
    public Mono<Profesional> actualizarProfesional(Profesional profesional) {
        return agendaRepository.actualizarProfesional(profesional);
    }

    public Flux<Cita> consultarCitasByTurnoCiudad (LocalDate fechaTurno, Integer idHorarioTurno, String idCiudad){
        return agendaRepository.consultarCitasByTurnoCiudad(fechaTurno, idHorarioTurno, idCiudad);
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
            String idCita, String idProfesional, LocalDate fechaTurno,Integer idHorarioTurno,String idRegional){
        return agendaRepository.agendarToProfesional( idCita, idProfesional, fechaTurno, idHorarioTurno, idRegional);
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
