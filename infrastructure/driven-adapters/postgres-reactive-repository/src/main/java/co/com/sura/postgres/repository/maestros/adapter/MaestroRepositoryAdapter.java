package co.com.sura.postgres.repository.maestros.adapter;

import co.com.sura.entity.maestro.*;
import co.com.sura.postgres.repository.maestros.data.CiudadRepository;
import co.com.sura.postgres.repository.maestros.data.HorarioTurnoRepository;
import co.com.sura.postgres.repository.maestros.data.PlanSaludRepository;
import co.com.sura.postgres.repository.maestros.data.TipoIdentificacionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@Repository
public class MaestroRepositoryAdapter implements MaestroRepository {

    @Autowired
    private CiudadRepository ciudadRepository;

    @Autowired
    private HorarioTurnoRepository horarioTurnoRepository;

    @Autowired
    private TipoIdentificacionRepository tipoIdentificacionRepository;

    @Autowired
    private PlanSaludRepository planSaludRepository;

    @Override
    public Flux<Ciudad> consultarCiudad() {
        return ciudadRepository.findAll()
                .map(ConverterMaestros::convertToCiudad);
    }

    @Override
    public Mono<Ciudad> consultarCiudadById(String idCiudad) {
        return ciudadRepository.findById(idCiudad)
                .map(ConverterMaestros::convertToCiudad);
    }

    @Override
    public Flux<HorarioTurno> consultarHorarioTurno() {
        return horarioTurnoRepository.findAll()
                .map(ConverterMaestros :: convertToHorarioTurno);
    }

    @Override
    public Mono<HorarioTurno> consultarHorarioTurnoById(Integer idHorarioTurno) {
         return horarioTurnoRepository.findById(idHorarioTurno)
                .map(ConverterMaestros :: convertToHorarioTurno);
    }

    @Override
    public Flux<TipoIdentificacion> consultarTipoIdentificacion() {
        return tipoIdentificacionRepository.findAll()
                .map(ConverterMaestros :: convertToTipoIdentificacion);
    }

    @Override
    public Mono<TipoIdentificacion> consultarTipoIdentificacionById(Integer idTipoIdentificacion) {
        return tipoIdentificacionRepository.findById(idTipoIdentificacion)
                .map(ConverterMaestros :: convertToTipoIdentificacion);
    }

    @Override
    public Flux<PlanSalud> consultarPlanSalud() {
        return planSaludRepository.findAll()
                .map(ConverterMaestros :: convertToTipoIdentificacion);
    }

    @Override
    public Mono<PlanSalud> consultarPlanSaludById(Integer idPlanSalud) {
        return planSaludRepository.findById(idPlanSalud)
                .map(ConverterMaestros :: convertToTipoIdentificacion);
    }
}
