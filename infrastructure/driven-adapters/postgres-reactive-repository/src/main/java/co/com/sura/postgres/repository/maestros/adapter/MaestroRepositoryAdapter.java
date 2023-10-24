package co.com.sura.postgres.repository.maestros.adapter;

import co.com.sura.entity.maestro.*;
import co.com.sura.postgres.repository.maestros.data.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.util.Comparator;


@Repository
public class MaestroRepositoryAdapter implements MaestroRepository {

    private final RegionalesRepository ciudadRepository;
    private final HorarioTurnoRepository horarioTurnoRepository;
    private final TipoIdentificacionRepository tipoIdentificacionRepository;
    private final EstadoCitaRepository estadoCitaRepository;
    private final ProfesionRepository profesionRepository;

    @Autowired
    public MaestroRepositoryAdapter(RegionalesRepository ciudadRepository,
                                    HorarioTurnoRepository horarioTurnoRepository,
                                    TipoIdentificacionRepository tipoIdentificacionRepository,
                                    EstadoCitaRepository estadoCitaRepository, ProfesionRepository profesionRepository){
        this.ciudadRepository = ciudadRepository;
        this.horarioTurnoRepository = horarioTurnoRepository;
        this.tipoIdentificacionRepository = tipoIdentificacionRepository;
        this.estadoCitaRepository = estadoCitaRepository;
        this.profesionRepository = profesionRepository;
    }

    @Override
    public Flux<Regional> consultarCiudad() {
        return ciudadRepository.findAll()
                .map(ConverterMaestros::convertToCiudad);
    }

    @Override
    public Mono<Regional> consultarCiudadById(String idCiudad) {
        return ciudadRepository.findById(idCiudad)
                .map(ConverterMaestros::convertToCiudad);
    }

    @Override
    public Flux<HorarioTurno> consultarHorarioTurno() {
        return horarioTurnoRepository.findAll()
                .map(ConverterMaestros :: convertToHorarioTurno)
                .collectList()
                .map(horarioTurnos -> {
                    horarioTurnos.sort(Comparator.comparing(HorarioTurno::getId));
                    return horarioTurnos;
                })
                .flatMapMany(Flux::fromIterable);
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
    public Flux<EstadoCita> consultarEstadosCita() {
        return estadoCitaRepository.findAll()
                .map(ConverterMaestros ::convertToEstadoCita);
    }

    @Override
    public Flux<Profesion> consultarProfesiones() {
        return profesionRepository.findAll()
                .map(ConverterMaestros::converToProfesionData);
    }
}
