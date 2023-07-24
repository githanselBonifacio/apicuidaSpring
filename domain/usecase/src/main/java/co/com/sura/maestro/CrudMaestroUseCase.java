package co.com.sura.maestro;

import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import co.com.sura.entity.maestro.MaestroFactory;
import co.com.sura.entity.maestro.MaestroRepository;
import co.com.sura.entity.maestro.Ciudad;
import co.com.sura.entity.maestro.HorarioTurno;
import co.com.sura.entity.maestro.TipoIdentificacion;
import co.com.sura.entity.maestro.PlanSalud;

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

    // Tipo Identificacion
    public Flux<PlanSalud> consultarPlanSalud(){
        return maestroRepository.consultarPlanSalud();
    }

    public Mono<PlanSalud> consultarPlanSaludById(Integer idPlanSalud){
        return maestroRepository.consultarPlanSaludById(idPlanSalud);
    }
}
