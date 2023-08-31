package co.com.sura.postgres.repository.maestros.adapter;

import co.com.sura.entity.maestro.Ciudad;
import co.com.sura.entity.maestro.HorarioTurno;
import co.com.sura.entity.maestro.TipoIdentificacion;
import co.com.sura.entity.maestro.EstadoCita;
import co.com.sura.entity.maestro.MaestroRepository;
import co.com.sura.postgres.repository.maestros.data.CiudadRepository;
import co.com.sura.postgres.repository.maestros.data.HorarioTurnoRepository;
import co.com.sura.postgres.repository.maestros.data.TipoIdentificacionRepository;
import co.com.sura.postgres.repository.maestros.data.EstadoCitaRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;


@Repository
public class MaestroRepositoryAdapter implements MaestroRepository {

    private final CiudadRepository ciudadRepository;
    private final HorarioTurnoRepository horarioTurnoRepository;
    private final TipoIdentificacionRepository tipoIdentificacionRepository;
    private final EstadoCitaRepository estadoCitaRepository;

    @Autowired
    public MaestroRepositoryAdapter(CiudadRepository ciudadRepository, HorarioTurnoRepository horarioTurnoRepository,
                                    TipoIdentificacionRepository tipoIdentificacionRepository,
                                     EstadoCitaRepository estadoCitaRepository){
        this.ciudadRepository = ciudadRepository;
        this.horarioTurnoRepository = horarioTurnoRepository;
        this.tipoIdentificacionRepository = tipoIdentificacionRepository;
        this.estadoCitaRepository = estadoCitaRepository;
    }

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
    public Flux<EstadoCita> consultarEstadosCita() {
        return estadoCitaRepository.findAll()
                .map(ConverterMaestros ::convertToEstadoCita);
    }
}
