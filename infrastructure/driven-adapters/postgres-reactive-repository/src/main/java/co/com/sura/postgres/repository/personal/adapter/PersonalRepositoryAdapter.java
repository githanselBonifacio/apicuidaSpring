package co.com.sura.postgres.repository.personal.adapter;

import co.com.sura.dto.request.EliminarTurnoProfesionalRequest;
import co.com.sura.entity.remision.ItemDiaTurno;
import co.com.sura.entity.remision.SecuenciaTurno;
import co.com.sura.entity.personal.Conductor;
import co.com.sura.entity.personal.Movil;
import co.com.sura.entity.personal.Profesional;
import co.com.sura.entity.personal.ProfesionalWithTurno;
import co.com.sura.entity.personal.TurnoProfesional;
import co.com.sura.entity.personal.PersonalRepository;
import co.com.sura.genericos.RespuestasFactory;
import co.com.sura.genericos.ResultadoActualizacionTurno;
import co.com.sura.postgres.repository.remision.adapter.RemisionDataFactory;
import co.com.sura.postgres.repository.remision.adapter.ConverterRemision;
import co.com.sura.postgres.repository.personal.data.ItemSecuenciaTurnoRepository;
import co.com.sura.postgres.repository.agenda.data.CitaRepository;
import co.com.sura.postgres.repository.personal.data.ConductorRepository;
import co.com.sura.postgres.repository.personal.data.MovilRepository;
import co.com.sura.postgres.repository.personal.data.ProfesionalRepository;
import co.com.sura.postgres.repository.personal.data.TurnoProfesionalesRepository;
import co.com.sura.postgres.repository.maestros.adapter.ConverterMaestros;
import co.com.sura.postgres.repository.maestros.data.HorarioTurnoRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.Comparator;
import java.util.List;

import static co.com.sura.constantes.Mensajes.*;
import static co.com.sura.constantes.Mensajes.RESPUESTA_TURNO;

@Repository
public class PersonalRepositoryAdapter implements PersonalRepository {

    private final CitaRepository citaRepository;
    private final ProfesionalRepository profesionalRepository;
    private final MovilRepository movilRepository;
    private final ConductorRepository conductorRepository;
    private final TurnoProfesionalesRepository turnoProfesionalesRepository;
    private final ItemSecuenciaTurnoRepository secuenciaTurnoRepository;
    private final HorarioTurnoRepository horarioTurnoRepository;

    @Autowired
    public PersonalRepositoryAdapter(CitaRepository citaRepository, ProfesionalRepository profesionalRepository,
                                     MovilRepository movilRepository, ConductorRepository conductorRepository,
                                     TurnoProfesionalesRepository turnoProfesionalesRepository,
                                     ItemSecuenciaTurnoRepository secuenciaTurnoRepository,
                                     HorarioTurnoRepository horarioTurnoRepository) {
        this.citaRepository = citaRepository;
        this.profesionalRepository = profesionalRepository;
        this.movilRepository = movilRepository;
        this.conductorRepository = conductorRepository;
        this.turnoProfesionalesRepository = turnoProfesionalesRepository;
        this.secuenciaTurnoRepository = secuenciaTurnoRepository;
        this.horarioTurnoRepository = horarioTurnoRepository;
    }

    //profesionales
    @Override
    public Flux<Profesional> consultarProfesionales() {
        return profesionalRepository.findAll()
                .map(ConverterRemision:: convertToProfesional)
                .sort(Comparator.comparing(Profesional::getNombres));
    }

    @Override
    public Flux<Profesional> consultarProfesionalesByRegional(String idRegional) {
        return profesionalRepository.findByIdRegional(idRegional)
                .map(ConverterRemision::convertToProfesional)
                .sort(Comparator.comparing(Profesional::getNombres));
    }

    @Override
    public Mono<Profesional> crearProfesional(Profesional profesional) {
        return profesionalRepository.existsById(profesional.getNumeroIdentificacion())
                .flatMap(exist ->  {
                    if (Boolean.TRUE.equals(exist)) {
                        return Mono.error(new Throwable(PROFESIONAL_YA_EXISTE.getValue()));
                    }
                    return profesionalRepository.insertProfesional(profesional);
                })
                .then(profesionalRepository.findById(profesional.getNumeroIdentificacion()))
                .map(ConverterRemision::convertToProfesional);
    }

    @Override
    public Mono<Profesional> actualizarProfesional(Profesional profesional) {
        return Mono.just(profesional)
                .then(profesionalRepository.existsById(profesional.getNumeroIdentificacion()))
                .flatMap(exist ->{
                    if (Boolean.FALSE.equals(exist)) {
                        return Mono.error(new Throwable(PROFESIONAL_NO_EXISTE.getValue()));
                    }
                    return profesionalRepository.save(ConverterRemision.convertToProfesionalData(profesional));
                })
                .then(profesionalRepository.findById(profesional.getNumeroIdentificacion()))
                .map(ConverterRemision::convertToProfesional);
    }
    //moviles
    @Override
    public Mono<Movil> crearMovil(Movil movil) {
        return movilRepository.existsById(movil.getMatricula())
                .flatMap(exist ->  {
                    if (Boolean.TRUE.equals(exist)) {
                        return Mono.error(new Throwable(MOVIL_YA_EXISTE.getValue()));
                    }
                    return movilRepository.insertMovil(movil);
                })
                .then(movilRepository.findById(movil.getMatricula()))
                .map(ConverterRemision::convertToMovil);
    }

    @Override
    public Mono<Movil> actualizarMovil(Movil movil) {
        return Mono.just(movil)
                .then(movilRepository.existsById(movil.getMatricula()))
                .flatMap(exist ->{
                    if (Boolean.FALSE.equals(exist)) {
                        return Mono.error(new Throwable(MOVIL_NO_EXISTE.getValue()));
                    }
                    return movilRepository.save(ConverterRemision.convertToMovilData(movil));
                })
                .then(movilRepository.findById(movil.getMatricula()))
                .map(ConverterRemision::convertToMovil);
    }

    @Override
    public Flux<Movil> consultarMoviles() {
        return movilRepository.findAll()
                .map(ConverterRemision::convertToMovil);
    }

    //conductores
    @Override
    public Flux<Movil> consultarMovilesSinConductor() {
        return movilRepository.findAllWithoutConductor()
                .map(ConverterRemision::convertToMovil);
    }

    @Override
    public Flux<Movil> consultarMovilesByIdRegional(String idRegional) {
        return movilRepository.findByIdRegional(idRegional)
                .map(ConverterRemision::convertToMovil);
    }

    @Override
    public Mono<Conductor> crearConductor(Conductor conductor) {
        return  conductorRepository.existsById(conductor.getNumeroIdentificacion())
                .flatMap(exist ->  {
                    if (Boolean.TRUE.equals(exist)) {
                        return Mono.error(new Throwable(CONDUCTOR_YA_EXISTE.getValue()));
                    }
                    return conductorRepository.insertConductor(conductor);
                })
                .then(conductorRepository.findById(conductor.getNumeroIdentificacion()))
                .map(ConverterRemision::converToConductor);
    }

    @Override
    public Mono<Conductor> actualizarConductor(Conductor conductor) {
        return Mono.just(conductor)
                .then(conductorRepository.existsById(conductor.getNumeroIdentificacion()))
                .flatMap(exist ->{
                    if (Boolean.FALSE.equals(exist)) {
                        return Mono.error(new Throwable(CONDUCTOR_NO_EXISTE.getValue()));
                    }
                    return conductorRepository.save(ConverterRemision.converToConductorData(conductor));
                })
                .then(conductorRepository.findById(conductor.getNumeroIdentificacion()))
                .map(ConverterRemision::converToConductor);
    }

    @Override
    public Flux<Conductor> consultarConductores() {
        return conductorRepository.findAll()
                .map(ConverterRemision::converToConductor);
    }

    //turnos del personal
    @Override
    public Flux<ProfesionalWithTurno> consultarHorariosProfesionales(String fechaTurno, String idRegional) {
        return profesionalRepository.findByIdRegional(idRegional)
                .map(ConverterRemision::convertToProfesionalTurno)
                .flatMap(profesional -> turnoProfesionalesRepository
                        .findTurnoProfesionalByFechaRegional(
                                fechaTurno,profesional.getNumeroIdentificacion(), profesional.getIdRegional())
                        .map(ConverterRemision::convertToTurnoProfesional)
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
                        .findCitasByTurnoProfesional(turno.getFechaTurno(),turno.getIdProfesional())
                        .hasElements()
                        .flatMap(hasElment ->{
                            if(Boolean.FALSE.equals(hasElment)){
                                return eliminarTurnosMasivamente(turno);
                            }
                            return  Mono.just(RespuestasFactory.crearResultadoActualizacionTurno(
                                    RESPUESTA_TURNO.getValue(),turno.getIdProfesional(), turno.getFechaTurno()));
                        })
                )
                .filter(ResultadoActualizacionTurno::isNotNull);

    }

    private Mono <ResultadoActualizacionTurno> asignarTurnosMasivamente(TurnoProfesional turno){
        return turnoProfesionalesRepository
                .eliminarByIdProfesionalFechaTurno(turno.getFechaTurno(),turno.getIdProfesional())
                .then(turnoProfesionalesRepository
                        .save(ConverterRemision.convertToTurnoProfesionalData(turno)))
                .then(Mono.just(ResultadoActualizacionTurno.builder().build()));
    }

    @Override
    public Flux<ResultadoActualizacionTurno> asignarTurnosProfesionalesAccionMasiva(List<TurnoProfesional> turnos) {
        return Flux.fromIterable(turnos)
                .flatMap(turno-> citaRepository
                        .findCitasByTurnoProfesional(turno.getFechaTurno(),turno.getIdProfesional())
                        .hasElements()
                        .flatMap(hasElment ->{
                            if(Boolean.FALSE.equals(hasElment)){
                                return asignarTurnosMasivamente(turno);
                            }
                            return Mono.just(RespuestasFactory.crearResultadoActualizacionTurno(
                                    RESPUESTA_TURNO.getValue(),turno.getIdProfesional(), turno.getFechaTurno()));
                        }))
                .filter(ResultadoActualizacionTurno::isNotNull);

    }


    @Override
    public Mono<Boolean> actualizarHorarioTurnoProfesionales(List<TurnoProfesional> turnos) {

        return turnoProfesionalesRepository.eliminarByIdProfesionalFechaTurno(
                        turnos.get(0).getFechaTurno(),turnos.get(0).getIdProfesional())
                .thenMany(Flux.fromIterable(turnos)
                        .map(ConverterRemision::convertToTurnoProfesionalData)
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
                .map(ConverterRemision::convertToSecuenciaTurno)
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
