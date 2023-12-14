package co.com.sura.maestro;

import co.com.sura.maestros.entity.EstadoCita;
import co.com.sura.maestros.entity.HorarioTurno;
import co.com.sura.maestros.gateway.MaestroRepository;
import co.com.sura.maestros.entity.Profesion;
import co.com.sura.maestros.entity.Regional;
import co.com.sura.maestros.entity.TipoIdentificacion;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

public class CrudMaestroUseCase {

    private final MaestroRepository maestroRepository;



    public CrudMaestroUseCase(MaestroRepository maestroRepository) {
        this.maestroRepository = maestroRepository;
    }
    //ciudades
    public Flux<Regional> consultarCiudad(){return maestroRepository.consultarRegional();}

    public Mono<Regional> consultarCiudadById(String idCiudad){
        return maestroRepository.consultarRegionalById(idCiudad);
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
}
