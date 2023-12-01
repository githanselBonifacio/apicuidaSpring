package co.com.sura.postgres.repository.personal.adapter;

import co.com.sura.constantes.Mensajes;
import co.com.sura.dto.request.EliminarTurnoProfesionalRequest;
import co.com.sura.entity.personal.Conductor;
import co.com.sura.entity.personal.Profesional;
import co.com.sura.entity.personal.ProfesionalWithTurno;
import co.com.sura.entity.personal.SecuenciasHorarioRepository;
import co.com.sura.entity.personal.TurnoProfesional;
import co.com.sura.entity.remision.datosremision.ItemDiaTurno;
import co.com.sura.entity.personal.SecuenciaTurno;
import co.com.sura.genericos.RespuestasFactory;
import co.com.sura.genericos.ResultadoActualizacionTurno;
import co.com.sura.postgres.repository.agenda.repository.CitaRepository;
import co.com.sura.postgres.repository.maestros.adapter.ConverterMaestros;
import co.com.sura.postgres.repository.maestros.repository.HorarioTurnoRepository;
import co.com.sura.postgres.repository.personal.repository.ItemSecuenciaTurnoRepository;
import co.com.sura.postgres.repository.personal.repository.ProfesionalRepository;
import co.com.sura.postgres.repository.personal.repository.TurnoProfesionalesRepository;
import co.com.sura.postgres.repository.remision.adapter.RemisionDataFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.Comparator;
import java.util.List;

@Repository
public class SecuenciasHorarioAdapter implements SecuenciasHorarioRepository {
    private final CitaRepository citaRepository;
    private final ProfesionalRepository profesionalRepository;
    private final TurnoProfesionalesRepository turnoProfesionalesRepository;
    private final ItemSecuenciaTurnoRepository secuenciaTurnoRepository;
    private final HorarioTurnoRepository horarioTurnoRepository;

    @Autowired
    public SecuenciasHorarioAdapter(CitaRepository citaRepository, ProfesionalRepository profesionalRepository,
                                    TurnoProfesionalesRepository turnoProfesionalesRepository,
                                    ItemSecuenciaTurnoRepository secuenciaTurnoRepository,
                                    HorarioTurnoRepository horarioTurnoRepository) {
        this.citaRepository = citaRepository;
        this.profesionalRepository = profesionalRepository;
        this.turnoProfesionalesRepository = turnoProfesionalesRepository;
        this.secuenciaTurnoRepository = secuenciaTurnoRepository;
        this.horarioTurnoRepository = horarioTurnoRepository;
    }

    //turnos del personal
    @Override
    public Flux<ProfesionalWithTurno> consultarHorariosProfesionales(String fechaTurno, String idRegional) {
        return profesionalRepository.findByIdRegional(idRegional)
                .map(ConverterPersonal::convertToProfesionalTurno)
                .flatMap(profesional -> turnoProfesionalesRepository
                        .findTurnoProfesionalByFechaRegional(
                                fechaTurno,profesional.getNumeroIdentificacion(), profesional.getIdRegional())
                        .map(ConverterPersonal::convertToTurnoProfesional)
                        .collectList()
                        .flatMap(turnos -> {
                            profesional.setTurnos(turnos);
                            return Mono.just(profesional);
                        }))
                .sort(Comparator.comparing(Profesional::getNombres));
    }

    private Mono <ResultadoActualizacionTurno> eliminarTurnosMasivamente(EliminarTurnoProfesionalRequest turno){
        return  turnoProfesionalesRepository
                .eliminarByIdProfesionalFechaTurno(turno.getFechaTurno(),turno.getIdProfesional())
                .then(Mono.just(ResultadoActualizacionTurno.builder().build()));
    }
    @Override
    public Flux<ResultadoActualizacionTurno> eliminarTurnosProfesionalesAccionMasiva(
            List<EliminarTurnoProfesionalRequest> turnoRequest) {
        return Flux.fromIterable(turnoRequest)
                .flatMap(turno ->citaRepository
                        .findAllByTurnoProfesional(turno.getFechaTurno(),turno.getIdProfesional())
                        .hasElements()
                        .flatMap(hasElment ->{
                          if(Boolean.FALSE.equals(hasElment)){
                                return eliminarTurnosMasivamente(turno);
                          }
                          return Mono.just(RespuestasFactory.crearResultadoActualizacionTurno(
                                 Mensajes.RESPUESTA_TURNO.getValue(),turno.getIdProfesional(), turno.getFechaTurno()));
                        })
                )
                .filter(ResultadoActualizacionTurno::isNotNull);

    }

    private Mono <ResultadoActualizacionTurno> asignarTurnosMasivamente(TurnoProfesional turno){
        return turnoProfesionalesRepository
                .eliminarByIdProfesionalFechaTurno(turno.getFechaTurno(),turno.getIdProfesional())
                .then(turnoProfesionalesRepository
                        .save(ConverterPersonal.convertToTurnoProfesionalData(turno)))
                .then(Mono.just(ResultadoActualizacionTurno.builder().build()));
    }

    @Override
    public Flux<ResultadoActualizacionTurno> asignarTurnosProfesionalesAccionMasiva(List<TurnoProfesional> turnos) {
        return Flux.fromIterable(turnos)
                .flatMap(turno-> citaRepository
                     .findAllByTurnoProfesional(turno.getFechaTurno(),turno.getIdProfesional())
                     .hasElements()
                     .flatMap(hasElment ->{
                        if(Boolean.FALSE.equals(hasElment)){
                           return asignarTurnosMasivamente(turno);
                        }
                        return Mono.just(RespuestasFactory.crearResultadoActualizacionTurno(
                                 Mensajes.RESPUESTA_TURNO.getValue(),turno.getIdProfesional(), turno.getFechaTurno()));
                        }))
                .filter(ResultadoActualizacionTurno::isNotNull);

    }


    @Override
    public Mono<Boolean> actualizarHorarioTurnoProfesionales(List<TurnoProfesional> turnos) {

        return turnoProfesionalesRepository.eliminarByIdProfesionalFechaTurno(
                        turnos.get(0).getFechaTurno(),turnos.get(0).getIdProfesional())
                .thenMany(Flux.fromIterable(turnos)
                        .map(ConverterPersonal::convertToTurnoProfesionalData)
                        .collectList()
                        .flatMapMany(turnoProfesionalesRepository::saveAll))
                .then(Mono.just(true));
    }
    @Override
    public Flux<Conductor> consultarHorariosConductores(LocalDate fechaTurno, String idRegional) {
        return null;
    }

    //secuencias turnos
    @Override
    public Flux<SecuenciaTurno> consultarSecuencias() {
        var horarioTurno  = horarioTurnoRepository.findAll()
                .map(ConverterMaestros::convertToHorarioTurno);
        return secuenciaTurnoRepository.findAll()
                .map(ConverterPersonal::convertToSecuenciaTurno)
                .flatMap(st -> {
                    var horariosTurnosFlux = horarioTurno
                            .filter(h -> h.getId().equals(st.getHorariosTurno().get(0).getId()))
                            .collectList();
                    return horariosTurnosFlux
                            .doOnNext(st::setHorariosTurno)
                            .thenReturn(st)
                            .map(SecuenciaTurno::crearSecuenciaTurnoFromItemsSecuencia);
                })
                .groupBy(SecuenciaTurno::getNombre)
                .flatMap(secuenciasTurnosAgrupados -> secuenciasTurnosAgrupados
                        .collectList()
                        .map(SecuenciaTurno::agruparItemsDiaTurno));

    }

    public Mono<Boolean> configurarSecuenciaTurno(SecuenciaTurno secuenciaTurno) {
        return secuenciaTurnoRepository.deleteByNombreSecuencia(secuenciaTurno.getNombre())
                .thenMany(Flux.fromIterable(secuenciaTurno.getItemsDiaTurno())
                        .map(ItemDiaTurno::inicializarListaHorarioTurnoVacio)
                        .flatMap(itemDiaTurno -> Flux.fromIterable(itemDiaTurno.getHorariosTurno())
                                .map(horarioTurno -> RemisionDataFactory.crearItemDiaTurnoData(
                                        secuenciaTurno,itemDiaTurno,horarioTurno)))
                        .collectList()
                        .flatMapMany(secuenciaTurnoRepository::saveAll))
                .then(Mono.just(true));
    }
}
