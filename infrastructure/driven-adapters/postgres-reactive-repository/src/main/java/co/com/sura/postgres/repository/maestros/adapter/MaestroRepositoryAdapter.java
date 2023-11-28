package co.com.sura.postgres.repository.maestros.adapter;

import co.com.sura.entity.maestro.EstadoCita;
import co.com.sura.entity.maestro.HorarioTurno;
import co.com.sura.entity.maestro.MaestroRepository;
import co.com.sura.entity.maestro.Profesion;
import co.com.sura.entity.maestro.Regional;
import co.com.sura.entity.maestro.TipoIdentificacion;
import co.com.sura.postgres.repository.maestros.data.EstadoCitaRepository;
import co.com.sura.postgres.repository.maestros.data.HorarioTurnoRepository;
import co.com.sura.postgres.repository.maestros.data.ProfesionRepository;
import co.com.sura.postgres.repository.maestros.data.RegionalesRepository;
import co.com.sura.postgres.repository.maestros.data.TipoIdentificacionRepository;
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
    public Flux<Regional> consultarRegional() {
        return ciudadRepository.findAll()
                .map(ConverterMaestros::convertToCiudad);
    }

    @Override
    public Mono<Regional> consultarRegionalById(String idCiudad) {
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
