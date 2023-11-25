package co.com.sura.personal;

import co.com.sura.dto.request.EliminarTurnoProfesionalRequest;
import co.com.sura.entity.personal.Conductor;
import co.com.sura.entity.personal.Movil;
import co.com.sura.entity.personal.PersonalCrudRepository;
import co.com.sura.entity.personal.Profesional;
import co.com.sura.entity.personal.ProfesionalWithTurno;
import co.com.sura.entity.personal.SecuenciasHorarioRepository;
import co.com.sura.entity.personal.TurnoProfesional;
import co.com.sura.entity.remision.SecuenciaTurno;
import co.com.sura.genericos.ResultadoActualizacionTurno;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;

public class PersonalUseCase {

    private final PersonalCrudRepository personalRepository;
    private final SecuenciasHorarioRepository secuenciasHorarioRepository;

    public PersonalUseCase(PersonalCrudRepository personalRepository,
                           SecuenciasHorarioRepository secuenciasHorarioRepository) {
        this.personalRepository = personalRepository;
        this.secuenciasHorarioRepository = secuenciasHorarioRepository;
    }

    //profesionales
    public Flux<Profesional> consultarProfesional() {
        return personalRepository.consultarProfesionales();
    }
    public Flux<Profesional> consultarProfesionalByRegional(String idRegional){
        return personalRepository.consultarProfesionalesByRegional(idRegional);
    }
    public Mono<Profesional> crearProfesional(Profesional profesional) {
        return personalRepository.crearProfesional(profesional);
    }
    public Mono<Profesional> actualizarProfesional(Profesional profesional) {
        return personalRepository.actualizarProfesional(profesional);
    }

    public  Flux<Conductor> consultarConductores(){
        return personalRepository.consultarConductores();
    }

    //conductor
    public Mono<Conductor> crearConductor(Conductor conductor){return personalRepository.crearConductor(conductor);}
    public Mono<Conductor> actualizarConductor(Conductor conductor){
        return personalRepository.actualizarConductor(conductor);
    }

    //moviles
    public Mono<Movil> crearMovil(Movil movil){return personalRepository.crearMovil(movil);}

    public Mono<Movil> actualizarMovil(Movil movil){
        return personalRepository.actualizarMovil(movil);
    }

    public Flux<Movil> consultarMoviles(){
        return personalRepository.consultarMoviles();
    }
    public Flux<Movil> consultarMovilesSinConductor(){
        return personalRepository.consultarMovilesSinConductor();
    }
    public Flux<Movil> consultarMovilesByIdRegional(String idRegional){
        return personalRepository.consultarMovilesByIdRegional(idRegional);
    }

    //turnos de profesionales
    public Flux<ProfesionalWithTurno> consultarProfesionalesTurnoByFechaTurnoIdRegional(
            String fechaTurno, String idRegional){
        return secuenciasHorarioRepository.consultarHorariosProfesionales(fechaTurno,idRegional);
    }
    public Flux<ResultadoActualizacionTurno> eliminarTurnosProfesionalAccionMasiva(
            List<EliminarTurnoProfesionalRequest> requests){
        return secuenciasHorarioRepository.eliminarTurnosProfesionalesAccionMasiva(requests);
    }

    public  Flux<ResultadoActualizacionTurno> asignarTurnosProfesionalAccionMasiva(List<TurnoProfesional>requests){
        return secuenciasHorarioRepository.asignarTurnosProfesionalesAccionMasiva(requests);
    }
    public Mono<Boolean> actualizarTurnosByProfesional(List<TurnoProfesional> turnos){
        return secuenciasHorarioRepository.actualizarHorarioTurnoProfesionales(turnos);
    }

    //secuencias de turno
    public Flux<SecuenciaTurno> consultarSecuenciasTurno(){
        return secuenciasHorarioRepository.consultarSecuencias();

    } public Mono<Boolean> configurarSecuenciaTurno(SecuenciaTurno secuenciaTurno){
        return secuenciasHorarioRepository.configurarSecuenciaTurno(secuenciaTurno);
    }
}
