package co.com.sura.postgres.repository.remision.adapter;

import co.com.sura.constantes.Mensajes;
import co.com.sura.dto.remision.CitaRequest;
import co.com.sura.dto.remision.NovedadRequest;
import co.com.sura.dto.remision.RemisionRequest;
import co.com.sura.entity.remision.datosremision.DatosAtencionPaciente;
import co.com.sura.entity.remision.datosremision.Paciente;
import co.com.sura.entity.remision.Remision;
import co.com.sura.entity.remision.RemisionCrudRepository;
import co.com.sura.entity.remision.historial.CitaHistorial;
import co.com.sura.exception.ErrorCitaProgreso;
import co.com.sura.exception.ErrorValidacionIngresoRemision;
import co.com.sura.genericos.EstadosCita;
import co.com.sura.postgres.repository.agenda.repository.CitaRepository;
import co.com.sura.postgres.repository.remision.repository.DatosAtencionPacienteRepository;
import co.com.sura.postgres.repository.remision.repository.PacienteRepository;
import co.com.sura.postgres.repository.remision.data.RemisionDiagnosticoData;
import co.com.sura.postgres.repository.remision.repository.RemisionDiagnosticoRepository;
import co.com.sura.postgres.repository.remision.repository.RemisionRepository;
import co.com.sura.postgres.repository.remision.repository.UbicacionRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.time.LocalDateTime;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static co.com.sura.constantes.Mensajes.*;


@Repository
public class RemisionRepositoryAdapter implements RemisionCrudRepository {

    private final RemisionRepository remisionRepository;
    private final UbicacionRepository ubicacionRepository;
    private final PacienteRepository pacienteRepository;
    private final PlanManejoRemisionAdapter planManejoRemisionAdapter;
    private final DatosAtencionPacienteRepository datosAtencionPacienteRepository;
    private final RemisionDiagnosticoRepository remisionDiagnosticoRepository;
    private final CitaRepository citaRepository;
    private final HistorialRemisionAdapter historialRemisionAdapter;

    @Autowired
    public RemisionRepositoryAdapter(RemisionRepository remisionRepository, UbicacionRepository ubicacionRepository,
                                     PacienteRepository pacienteRepository,
                                     PlanManejoRemisionAdapter planManejoRemisionAdapter,
                                     DatosAtencionPacienteRepository datosAtencionPacienteRepository,
                                     RemisionDiagnosticoRepository remisionDiagnosticoRepository,
                                     CitaRepository citaRepository,
                                     HistorialRemisionAdapter historialRemisionAdapter) {

        this.remisionRepository = remisionRepository;
        this.ubicacionRepository = ubicacionRepository;
        this.pacienteRepository = pacienteRepository;
        this.planManejoRemisionAdapter = planManejoRemisionAdapter;
        this.datosAtencionPacienteRepository = datosAtencionPacienteRepository;
        this.remisionDiagnosticoRepository = remisionDiagnosticoRepository;
        this.citaRepository = citaRepository;
        this.historialRemisionAdapter = historialRemisionAdapter;
    }

    @Override
    public Flux<Remision> consultarRemisiones() {
        return remisionRepository.findAllRemision();
    }

    //Crear y actualizar remisiones
    protected Mono<Void> registrarPacienteRemision(RemisionRequest remisionRequest, boolean esNovedad){
        var ubicacionData = ConverterRemision.extraerUbicacionData(remisionRequest);
        var pacienteData = ConverterRemision.extraerPacienteData(remisionRequest);

        if(esNovedad){
            return Mono.from(ubicacionRepository.save(ubicacionData))
               .then(pacienteRepository.save(pacienteData))
               .then();
        }else {
            return Mono.from(pacienteRepository.existsById(pacienteData.getNumeroIdentificacion()))
               .flatMap(pacienteExiste ->{
                   Mono<Boolean> inserts = Mono.just(Boolean.TRUE);
                 if (Boolean.FALSE.equals(pacienteExiste)){
                      inserts = ubicacionRepository.insertNuevaUbicacion(ubicacionData.getIdUbicacion())
                             .then(pacienteRepository.insertNuevoPaciente(pacienteData.getNumeroIdentificacion()));

                 }
                 return inserts
                           .then(Mono.from(ubicacionRepository.save(ubicacionData)))
                           .then(Mono.from(pacienteRepository.save(pacienteData))).then();
               });
        }
    }

    protected  Mono<Void> registrarDatosRemision(RemisionRequest remisionRequest, boolean esNovedad){
        var remisionData = ConverterRemision.convertToRemisionRequest(remisionRequest);
        var datosAtencionPacienteData = ConverterRemision
                .extraerDatosAtencionPacienteData(
                        remisionRequest.getDatosAtencionPaciente(), remisionRequest.getIdRemision());

        List<RemisionDiagnosticoData> diagnosticosData = ConverterRemision
                .extraerRemisionDiagnosticoData(remisionRequest.getDiagnosticos(), remisionRequest.getIdRemision());

       if(esNovedad){
           return   Mono.from(Mono.from(remisionRepository.save(remisionData)))
                   .then(Mono.from(datosAtencionPacienteRepository.updateDatosAtencion(datosAtencionPacienteData)))
                   .then(Mono.from(remisionDiagnosticoRepository.updateMultiplesDiagnosticos(diagnosticosData)))
                   .then();

       }else {
           return  Mono.from(Mono.from(remisionRepository.insertNuevaRemision(remisionData.getIdRemision())))
                   .then(Mono.from(Mono.from(remisionRepository.save(remisionData))))
                   .then(Mono.from(datosAtencionPacienteRepository.save(datosAtencionPacienteData)))
                   .then(Mono.from(remisionDiagnosticoRepository.insertMultiplesDiagnosticos(diagnosticosData)))
                   .then();
       }
    }

    @Override
    public Mono<Boolean> crearRemisionCita(RemisionRequest remisionRequest, List<CitaRequest> citasRequest) {

      String idRemision = remisionRequest.getIdRemision();
      return remisionRepository.existsById(idRemision)
         .flatMap(validacion -> {
                if (Boolean.TRUE.equals(validacion)) {
                    return Mono.error(new Throwable(Mensajes.REMISION_EXISTENTE.getValue().replace("?", idRemision)));
                }
                return Mono.just(Boolean.TRUE);
         })
         .then(registrarPacienteRemision(remisionRequest,false)
            .then(registrarDatosRemision(remisionRequest,false))
            .then(planManejoRemisionAdapter.registrarPlanManejo(remisionRequest,citasRequest,0))
            .onErrorResume(e-> eliminarDatosPacienteRemision(idRemision)
                  .then(planManejoRemisionAdapter.eliminarPlanManejoByidRemision(idRemision))
                  .then(Mono.error(e))))
            .then(Mono.just(Boolean.TRUE));
    }
    @Override
    public Mono<Boolean> actualizarRemisionPorNovedad(RemisionRequest remisionRequest, List<CitaRequest> citasRequest,
                                                      NovedadRequest novedadRequest) {

        String idRemision = remisionRequest.getIdRemision();
        LocalDateTime fechaAplicacionNovedad = novedadRequest.getFechaAplicarNovedad();

        citasRequest.removeIf(
                citaRequest -> citaRequest.getFechaInicio()
                        .isBefore(novedadRequest.getFechaAplicarNovedad()));

        var ultimoId= new AtomicInteger();
        return remisionRepository.existsById(idRemision)
            .flatMap(validacion->{
                if(Boolean.FALSE.equals(validacion)){
                    return Mono.error(new Throwable(REMISION_NO_EXISTENTE.getValue().replace("?", idRemision)));
                }
                    return Mono.just(Boolean.TRUE);
            })
            .then(Mono.zip(
                citaRepository.findLastNumberIdCita(idRemision).defaultIfEmpty(0),
                historialRemisionAdapter.buildRegistroActualRemision(idRemision, fechaAplicacionNovedad)))
            .map(tuple->{
                ultimoId.set(tuple.getT1());
                List<CitaHistorial> citasNuevas = ConverterRemision
                        .buildCitaHistorialFromRequest(citasRequest,remisionRequest,ultimoId.get());
                tuple.getT2().setMotivoNovedad(novedadRequest.getMotivoNovedad());
                tuple.getT2().setFechaAplicacionNovedad(novedadRequest.getFechaAplicarNovedad());
                tuple.getT2().setFechaRegistro(LocalDateTime.now());
                tuple.getT2().setCitasNuevas(ConverterRemision.convertToJsonb(citasNuevas));
                return tuple.getT2();
            })
            .flatMap(historialRemisionAdapter::insertRegistro)
            .then(Mono.from(citaRepository
                        .deleteCitaDataByIdRemision(idRemision, novedadRequest.getFechaAplicarNovedad())))
            .then(registrarPacienteRemision(remisionRequest, true))
            .then(registrarDatosRemision(remisionRequest, true))
            .then(Mono.just(citasRequest.isEmpty())
                    .flatMap(validarCitas-> {
                        if (Boolean.FALSE.equals(validarCitas)){
                            return planManejoRemisionAdapter
                                    .registrarPlanManejo(remisionRequest, citasRequest,  ultimoId.get());
                        }
                        return Mono.just(Boolean.TRUE);
                    }))
            .then(Mono.just(Boolean.TRUE));
    }


    @Override
    public Mono<Boolean> egresarRemisionById(String idRemision) {
        return remisionRepository.validarEstadosRemisionToEgreso(idRemision)
           .flatMap(exists -> {
               if (Boolean.FALSE.equals(exists)) {
                 return Mono.error(new ErrorValidacionIngresoRemision(
                         REMISION_NO_EXISTENTE.getValue().replace("?", idRemision)));
               }
               return Mono.just(true);

           })
           .then(citaRepository.validarEstadosToEgreso(
                   idRemision,EstadosCita.CONFIRMADA.getEstado(),EstadosCita.EN_PROGRESO.getEstado())
                .flatMap(valid -> {
                      if (Boolean.TRUE.equals(valid)) {
                            return Mono.error(new ErrorCitaProgreso(
                                           REMISION_CITAS_PROGRESO.getValue().replace("?", idRemision)));
                      }
                      return Mono.just(true);
                }))
           .then(citaRepository.cancelarToEgreso(
                   idRemision,EstadosCita.CANCELADA.getEstado(),
                   EstadosCita.SIN_AGENDAR.getEstado(),EstadosCita.AGENDADA.getEstado()))
           .then(remisionRepository.egresarRemisionById(idRemision))
           .then(Mono.just(true))
           .onErrorResume(Mono::error);
    }

    //datos paciente
    @Override
    public Mono<DatosAtencionPaciente> consultarDatosAtencionPacienteByIdRemision(String idRemision) {
        return datosAtencionPacienteRepository.findByIdRemision(idRemision)
                .map(ConverterRemision:: convertToDatosAtencionPaciente);
    }

    @Override
    public Mono<Paciente> consultarPacienteFromRemision(String idRemision) {
        return pacienteRepository.findPacienteByNumeroIdRemision(idRemision)
                .flatMap(pacienteData -> ubicacionRepository.findById(pacienteData.getIdUbicacion())
                        .map(ubicacionData -> {
                            var paciente = ConverterRemision.convertToPaciente(pacienteData);
                            paciente.setUbicacion(ConverterRemision.convertToUbicacion(ubicacionData));
                            return paciente;
                        }));
    }

    public Mono<Void> eliminarDatosPacienteRemision(String idRemision){
        return datosAtencionPacienteRepository.deleteByIdRemision(idRemision)
                .then(remisionDiagnosticoRepository.deleteByIdRemision(idRemision))
                .then(remisionRepository.deleteById(idRemision));
    }

}
