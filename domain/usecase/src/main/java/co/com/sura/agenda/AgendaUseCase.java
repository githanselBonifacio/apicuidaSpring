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

    public Flux<Profesional> consultarProfesionalesByTurnoCiudad(LocalDate fechaTurno, String idCiudad) {
        return agendaRepository.consultarProfesionalByTurnoCiudad(fechaTurno,idCiudad);
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

    public Mono<Boolean> desagendarTurnoCompleto(LocalDate fechaTurno,Integer idHorarioTurno,String idCiudad){
        return agendaRepository.desagendarTurnocompleto(fechaTurno,idHorarioTurno,idCiudad);
    }

    public Flux<Profesional> consultarProfesionalesByCiudad(String idCiudad) {
        return agendaRepository.consultarProfesionalesByIdCiudad(idCiudad);
    }

    public Flux<Actividad> consultarActividadesProfesionalesCiudadHorario(
          LocalDate fechaTurno,
          Integer idHorarioTurno,
          String idCiudad) {
        return agendaRepository
                .consultarActividadesByProfesionalesCiudadHorarioTurno(fechaTurno,idHorarioTurno,idCiudad);
    }
     public Mono<Boolean> autoagendarTurnoCompleto( LocalDate fechaTurno,
                                                 Integer idHorarioTurno,
                                                 String idCiudad){
        return agendaRepository.autoagendarTurnoCompleto(fechaTurno,idHorarioTurno,idCiudad);
     }

    public Flux<Desplazamiento> consultarDesplazamientoByIdCitaPartida(
            LocalDate fechaProgramada, Integer idHorarioTurno,String idCiudad){
        return agendaRepository.consultarDesplazamientoByCitaPartida(
                fechaProgramada,idHorarioTurno,idCiudad);
    }
    public Mono<Boolean> calcularDesplazamientoCitaByProfesional(
            LocalDate fechaProgramada, Integer idHorarioTurno, String idCiudad, String idProfesional){
        return agendaRepository.calcularDesplazamientoCitaByProfesional(
                fechaProgramada,idHorarioTurno,idCiudad,idProfesional
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
    public Mono<Boolean> reprogramarCitaById (LocalDateTime fechaProgramada, String idCita){
        return agendaRepository.reprogramarCita(fechaProgramada, idCita);
    }

    public Mono<Boolean> asignarProfesionaCita (String idCita, String idProfesional){
        return agendaRepository.agendarToProfesional(idCita,idProfesional);
    }

    public Mono<Boolean> desasignarProfesionaCita (String idCita){
        return agendaRepository.desagendarToProfesional(idCita);
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
