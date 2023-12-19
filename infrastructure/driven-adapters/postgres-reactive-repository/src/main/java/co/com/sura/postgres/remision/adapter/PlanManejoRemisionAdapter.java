package co.com.sura.postgres.remision.adapter;

import co.com.sura.postgres.agenda.data.CitaData;
import co.com.sura.postgres.agenda.repository.CitaRepository;
import co.com.sura.postgres.maestros.data.HorarioTurnoData;
import co.com.sura.postgres.maestros.repository.HorarioTurnoRepository;
import co.com.sura.remision.dto.CitaRequest;
import co.com.sura.remision.dto.RemisionRequest;
import co.com.sura.remision.gateway.PlanManejoRemisionRepository;
import co.com.sura.postgres.remision.repository.procedimientos.CanalizacionRepository;
import co.com.sura.postgres.remision.repository.procedimientos.CuracionRepository;
import co.com.sura.postgres.remision.repository.procedimientos.FototerapiaRepository;
import co.com.sura.postgres.remision.repository.procedimientos.SecrecionRepository;
import co.com.sura.postgres.remision.repository.procedimientos.SondajeRepository;
import co.com.sura.postgres.remision.repository.procedimientos.SoporteNutricionalRepository;
import co.com.sura.postgres.remision.repository.procedimientos.TomaMuestraRepository;
import co.com.sura.postgres.remision.repository.tratamientos.TratamientoRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.List;


@Repository
public class PlanManejoRemisionAdapter implements PlanManejoRemisionRepository {

    private final HorarioTurnoRepository horarioTurnoRepository;
    private final CitaRepository citaRepository;
    private final TratamientoRepository tratamientoRepository;
    private final CanalizacionRepository canalizacionRepository;
    private final SecrecionRepository secrecionRepository;
    private final CuracionRepository curacionRepository;
    private final FototerapiaRepository fototerapiaRepository;
    private final SondajeRepository sondajeRepository;
    private final TomaMuestraRepository tomaMuestraRepository;
    private final SoporteNutricionalRepository soporteNutricionalRepository;

    @Autowired
    public PlanManejoRemisionAdapter(HorarioTurnoRepository horarioTurnoRepository, CitaRepository citaRepository,
                                     TratamientoRepository tratamientoRepository,
                                     CanalizacionRepository canalizacionRepository,
                                     SecrecionRepository secrecionRepository,
                                     CuracionRepository curacionRepository, FototerapiaRepository fototerapiaRepository,
                                     SondajeRepository sondajeRepository, TomaMuestraRepository tomaMuestraRepository,
                                     SoporteNutricionalRepository soporteNutricionalRepository) {

        this.horarioTurnoRepository = horarioTurnoRepository;
        this.citaRepository = citaRepository;
        this.tratamientoRepository = tratamientoRepository;
        this.canalizacionRepository = canalizacionRepository;
        this.secrecionRepository = secrecionRepository;
        this.curacionRepository = curacionRepository;
        this.fototerapiaRepository = fototerapiaRepository;
        this.sondajeRepository = sondajeRepository;
        this.tomaMuestraRepository = tomaMuestraRepository;
        this.soporteNutricionalRepository = soporteNutricionalRepository;
    }

    /**
     registrar plan de manejo
     */
    @Override
    public   Mono<Void> registrarPlanManejo(RemisionRequest remisionRequest, List<CitaRequest> citasRequest,
                                            Integer ultimoIdCita){

        return Mono.just(
                Flux.range(1,citasRequest.size())
                    .map(i -> {
                       var citaRequest = citasRequest.get(i-1);
                                citaRequest.setIdCita(remisionRequest.getIdRemision()+"-"+(ultimoIdCita+i));
                       return citaRequest;
                    }))
                .flatMap(Flux::collectList)

                .flatMap(citasRequests -> Mono.zip(
                         Mono.just(ConverterRemision.convertirCitasDataList(citasRequest, remisionRequest)),
                         Mono.just(citasRequests)))

                .flatMapMany(tupleCita -> citaRepository.insertMultiplescitas(tupleCita.getT1())
                        .thenMany(this.asignarTurnoCitas(Flux.fromIterable(tupleCita.getT1()))
                                        .flatMap(citaRepository::save))
                        .thenMany(Flux.fromIterable(tupleCita.getT2())))

                .collectList()
                .flatMapMany(citasRequests -> Mono.zip(
                                Mono.just(ConverterRemision.extraerTratamientoData(citasRequests)),
                                Mono.just(ConverterRemision.extraerCanalizacionData(citasRequests)),
                                Mono.just(ConverterRemision.extraerFototerapiaData(citasRequests)),
                                Mono.just(ConverterRemision.extraerSecrecionData(citasRequests)),
                                Mono.just(ConverterRemision.extraerSondajeData(citasRequests)),
                                Mono.just(ConverterRemision.extraerSoporteNutricionalData(citasRequests)),
                                Mono.just(ConverterRemision.extraerSoporteTomaMuestraData(citasRequests)),
                                Mono.just(ConverterRemision.extraerCuracionData(citasRequests))))

                .flatMap(tupleProc->
                        tratamientoRepository.saveAll(tupleProc.getT1())
                        .thenMany(canalizacionRepository.saveAll(tupleProc.getT2()))
                        .thenMany(fototerapiaRepository.saveAll(tupleProc.getT3()))
                        .thenMany(secrecionRepository.saveAll(tupleProc.getT4()))
                        .thenMany(sondajeRepository.saveAll(tupleProc.getT5()))
                        .thenMany(soporteNutricionalRepository.saveAll(tupleProc.getT6()))
                        .thenMany(tomaMuestraRepository.saveAll(tupleProc.getT7()))
                        .thenMany(curacionRepository.saveAll(tupleProc.getT8())))
                .then();
    }
    private Flux<CitaData> asignarTurnoCitas(Flux <CitaData> citas){
        return    horarioTurnoRepository.findAll()
                .filter(HorarioTurnoData::getEsHorarioBase)
                .collectList()
                .flatMapMany(horariosTurno -> citas.map(cita->{
                      HorarioTurnoData horario =  horariosTurno.stream()
                       .filter(horarioTurnoData ->
                           cita.getFechaInicio().toLocalTime().plusMinutes(1).isAfter(horarioTurnoData.getHoraInicio())
                           && cita.getFechaInicio().toLocalTime().isBefore(horarioTurnoData.getHoraFin()))
                       .findFirst()
                       .orElse(HorarioTurnoData.builder().build());

                      cita.setIdHorarioTurno(horario.getId());
                      return cita;
                     })
                );

    }
    public Mono<Void> eliminarPlanManejoByidRemision(String idRemision){
        return soporteNutricionalRepository.deleteByIDRemision(idRemision)
                .then(tomaMuestraRepository.deleteByIDRemision(idRemision))
                .then(sondajeRepository.deleteByIDRemision(idRemision))
                .then(fototerapiaRepository.deleteByIDRemision(idRemision))
                .then(curacionRepository.deleteByIDRemision(idRemision))
                .then(secrecionRepository.deleteByIDRemision(idRemision))
                .then(canalizacionRepository.deleteByIDRemision(idRemision));
    }
}
