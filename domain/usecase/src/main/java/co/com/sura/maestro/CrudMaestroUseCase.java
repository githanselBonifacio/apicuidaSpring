package co.com.sura.maestro;

import co.com.sura.entity.maestro.*;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public class CrudMaestroUseCase implements MaestroFactory {

    private final MaestroRepository maestroRepository;



    public CrudMaestroUseCase(MaestroRepository maestroRepository) {
        this.maestroRepository = maestroRepository;
    }
    //ciudades
    public Flux<Ciudad> consultarCiudad(){return maestroRepository.consultarCiudad();}

    public Mono<Ciudad> consultarCiudadById(String idCiudad){
        return maestroRepository.consultarCiudadById(idCiudad);
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
}
