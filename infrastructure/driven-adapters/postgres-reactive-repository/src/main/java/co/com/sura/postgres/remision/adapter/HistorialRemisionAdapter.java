package co.com.sura.postgres.remision.adapter;

import co.com.sura.constantes.Mensajes;
import co.com.sura.agenda.entity.Cita;
import co.com.sura.postgres.remision.repository.datospaciente.PacienteRepository;
import co.com.sura.remision.gateway.HistorialRemisionRepository;
import co.com.sura.remision.entity.historial.RegistroHistorialRemision;
import co.com.sura.postgres.agenda.adapter.ConverterAgenda;
import co.com.sura.postgres.agenda.adapter.ProcedimientosCitasAdapter;
import co.com.sura.postgres.agenda.repository.CitaRepository;
import co.com.sura.postgres.remision.data.datospaciente.RegistroHistorialRemisionData;
import co.com.sura.postgres.remision.repository.datospaciente.DatosAtencionPacienteRepository;
import co.com.sura.postgres.remision.repository.datospaciente.RegistroHistorialRepository;
import co.com.sura.postgres.remision.repository.datospaciente.RemisionDiagnosticoRepository;
import co.com.sura.postgres.remision.repository.datospaciente.RemisionRepository;
import co.com.sura.postgres.remision.repository.tratamientos.TratamientoRepository;
import co.com.sura.postgres.remision.repository.datospaciente.UbicacionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.time.LocalDateTime;
import java.util.Comparator;


@Repository
public class HistorialRemisionAdapter implements HistorialRemisionRepository {
    private final RemisionRepository remisionRepository;
    private final PacienteRepository pacienteRepository;
    private final UbicacionRepository ubicacionRepository;
    private final DatosAtencionPacienteRepository datosAtencionPacienteRepository;
    private final RemisionDiagnosticoRepository diagnosticoRepository;
    private final TratamientoRepository tratamientoRepository;
    private final ProcedimientosCitasAdapter procedimientosCitasAdapter;
    private final CitaRepository citaRepository;
    private final RegistroHistorialRepository registroHistorialRemisionRepository;

    @Autowired
    public HistorialRemisionAdapter(RemisionRepository remisionRepository,
                                    PacienteRepository pacienteRepository,
                                    UbicacionRepository ubicacionRepository,
                                    DatosAtencionPacienteRepository datosAtencionPacienteRepository,
                                    RemisionDiagnosticoRepository diagnosticoRepository,
                                    TratamientoRepository tratamientoRepository,
                                    ProcedimientosCitasAdapter procedimientosCitasAdapter,
                                    CitaRepository citaRepository,
                                    RegistroHistorialRepository registroHistorialRemisionRepository) {
        this.remisionRepository = remisionRepository;
        this.pacienteRepository = pacienteRepository;
        this.ubicacionRepository = ubicacionRepository;
        this.datosAtencionPacienteRepository = datosAtencionPacienteRepository;
        this.diagnosticoRepository = diagnosticoRepository;
        this.tratamientoRepository = tratamientoRepository;
        this.procedimientosCitasAdapter = procedimientosCitasAdapter;
        this.citaRepository = citaRepository;


        this.registroHistorialRemisionRepository = registroHistorialRemisionRepository;
    }

    public Mono<Void> insertRegistro(RegistroHistorialRemisionData registroHistorialRemisionData){
        return registroHistorialRemisionRepository.save(registroHistorialRemisionData)
                .then();
    }
    public  Mono<RegistroHistorialRemisionData> buildRegistroActualRemision(
            String idRemision, LocalDateTime fechaAplicarNovedad){

        return consultarDatosRemisionActual(idRemision,fechaAplicarNovedad)
                .map(ConverterRemision::convertToRegistroHistoriaRemisionData);
    }

    @Override
    public Flux<RegistroHistorialRemision> consultarHistoricoRemision(String idRemision) {
        return registroHistorialRemisionRepository.findAllByIdRemision(idRemision)
                .map(ConverterRemision::convertToRegistroHistoriaRemision);
    }

    @Override
    public Mono<RegistroHistorialRemision> consultarDatosRemision(String idRemision) {
        return remisionRepository.findById(idRemision)
                .switchIfEmpty(Mono.error(new Throwable(Mensajes.REMISION_NO_EXISTENTE
                        .replace("?",idRemision))))
                .flatMap(remisionData ->
                        consultarDatosRemisionActual(idRemision, remisionData.getFechaAdmision().atStartOfDay()));
    }

    private Mono<RegistroHistorialRemision> consultarDatosRemisionActual(
            String idRemision , LocalDateTime fechaBusquedaCita){
        return remisionRepository.findById(idRemision)
                .map(ConverterRemision::buildHistorialRemision)
                .flatMap(registroRemisionData -> Mono.zip(
                        Mono.just(registroRemisionData),
                        pacienteRepository.findPacienteByNumeroIdRemision(idRemision)
                                .map(ConverterRemision::convertToPaciente),
                        ubicacionRepository
                                .findByIdRemision(registroRemisionData.getPaciente().getNumeroIdentificacion()),
                        datosAtencionPacienteRepository.findByIdRemision(idRemision),
                        diagnosticoRepository.findAllByIdRemision(idRemision).collectList()))

                .map(tupleDatosRemision-> tupleDatosRemision.getT1()
                        .toBuilder()
                        .paciente(tupleDatosRemision.getT2())
                        .ubicacionPaciente(tupleDatosRemision.getT3())
                        .datosAtencion(tupleDatosRemision.getT4())
                        .diagnosticos(tupleDatosRemision.getT5())
                        .build())

                .flatMap(registroRemision-> Mono.zip(
                        Mono.just(registroRemision),
                        citaRepository.findAllByIdRemision(idRemision)
                                .filter(citaData -> citaData.getFechaProgramada().isAfter(fechaBusquedaCita))
                                .map(ConverterAgenda::convertToCitaHistorial)
                                .sort(Comparator.comparing(Cita::getFechaProgramada))
                                .collectList()))

                .map(tupleCita-> tupleCita.getT1().toBuilder()
                        .citas(tupleCita.getT2())
                        .build())

                .map(registroRemision -> Flux.fromIterable(registroRemision.getCitas())
                   .flatMapSequential(cita -> Mono.zip(
                         procedimientosCitasAdapter.consultarProcedimientosByIdCita(cita.getIdCita()),
                         tratamientoRepository.findByIdCita(cita.getIdCita())
                                 .map(ConverterRemision::converterToTratamiento).collectList())
                            .doOnNext(tuplaPlaManejo -> {
                                cita.setProcedimientos(tuplaPlaManejo.getT1());
                                cita.setTratamientos(tuplaPlaManejo.getT2());
                            }).thenReturn(cita))
                   .collectList()
                   .map(citasAgregadas -> registroRemision.toBuilder().citas(citasAgregadas).build()))
                .flatMap(registroHistorialRemisionMono -> registroHistorialRemisionMono);
    }

}
