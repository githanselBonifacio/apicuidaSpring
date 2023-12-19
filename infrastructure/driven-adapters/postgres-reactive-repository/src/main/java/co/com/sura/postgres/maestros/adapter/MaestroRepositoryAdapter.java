package co.com.sura.postgres.maestros.adapter;

import co.com.sura.maestros.entity.EstadoCita;
import co.com.sura.maestros.entity.HorarioTurno;
import co.com.sura.maestros.gateway.MaestroRepository;
import co.com.sura.maestros.entity.Profesion;
import co.com.sura.maestros.entity.Regional;
import co.com.sura.maestros.entity.TipoIdentificacion;
import co.com.sura.postgres.maestros.repository.EstadoCitaRepository;
import co.com.sura.postgres.maestros.repository.HorarioTurnoRepository;
import co.com.sura.postgres.maestros.repository.ProfesionRepository;
import co.com.sura.postgres.maestros.repository.RegionalesRepository;
import co.com.sura.postgres.maestros.repository.TipoIdentificacionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.util.Comparator;


@Repository
public class MaestroRepositoryAdapter implements MaestroRepository {

    private final RegionalesRepository regionalesRepository;
    private final HorarioTurnoRepository horarioTurnoRepository;
    private final TipoIdentificacionRepository tipoIdentificacionRepository;
    private final EstadoCitaRepository estadoCitaRepository;
    private final ProfesionRepository profesionRepository;

    @Autowired
    public MaestroRepositoryAdapter(RegionalesRepository regionalesRepository,
                                    HorarioTurnoRepository horarioTurnoRepository,
                                    TipoIdentificacionRepository tipoIdentificacionRepository,
                                    EstadoCitaRepository estadoCitaRepository, ProfesionRepository profesionRepository){
        this.regionalesRepository = regionalesRepository;
        this.horarioTurnoRepository = horarioTurnoRepository;
        this.tipoIdentificacionRepository = tipoIdentificacionRepository;
        this.estadoCitaRepository = estadoCitaRepository;
        this.profesionRepository = profesionRepository;
    }

    @Override
    public Flux<Regional> consultarRegional() {
        return regionalesRepository.findAll()
                .map(ConverterMaestros::convertToRegional);
    }

    @Override
    public Mono<Regional> consultarRegionalById(String idRegional) {
        return regionalesRepository.findById(idRegional)
                .map(ConverterMaestros::convertToRegional);
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
