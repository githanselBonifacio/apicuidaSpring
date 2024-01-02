package co.com.sura.maestro;

import co.com.sura.maestros.entity.*;
import co.com.sura.maestros.gateway.MaestroRepository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public class CrudMaestroUseCase {

    private final MaestroRepository maestroRepository;



    public CrudMaestroUseCase(MaestroRepository maestroRepository) {
        this.maestroRepository = maestroRepository;
    }
    //ciudades
    public Flux<Regional> consultarRegionales(){return maestroRepository.consultarRegional();}

    public Mono<Regional> consultarRegionalById(String idRegional){
        return maestroRepository.consultarRegionalById(idRegional);
    }

    //horario turno
    public Flux<HorarioTurno> consultarHorarioTurno(){return maestroRepository.consultarHorarioTurno();}

    public Mono<HorarioTurno> consultarHorarioTurnoById(Integer idHorarioTurno){
        return maestroRepository.consultarHorarioTurnoById(idHorarioTurno);
    }

    // Tipo Identificacion
    public Flux<TipoIdentificacion> consultarTipoIdentificacion(){
        return maestroRepository.consultarTipoIdentificacion();
    }

    public Mono<TipoIdentificacion> consultarTipoIdentificacionById(Integer idTipoIdentificacion){
        return maestroRepository.consultarTipoIdentificacionById(idTipoIdentificacion);
    }


    public Flux<EstadoCita> consultarEstadosCita(){return  maestroRepository.consultarEstadosCita();}

    //profesiones
    public Flux<Profesion> consultarProfesiones(){return  maestroRepository.consultarProfesiones();}
    public Flux<MotivoCancelacionCita> consultarMotivosCancelacionCita(){
        return  maestroRepository.consultarMotivosCancelacionCita();}
}
