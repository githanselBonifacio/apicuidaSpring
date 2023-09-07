package co.com.sura.postgres.repository.remision.adapter;

import co.com.sura.constantes.Mensajes;
import co.com.sura.dto.remision.CitaRequest;
import co.com.sura.dto.remision.NovedadRequest;
import co.com.sura.dto.remision.RemisionRequest;
import co.com.sura.entity.agenda.PacienteTratamientoCita;
import co.com.sura.entity.remision.Remision;
import co.com.sura.entity.remision.RemisionCrudRepository;
import co.com.sura.entity.remision.Paciente;
import co.com.sura.entity.remision.DatosAtencionPaciente;
import co.com.sura.entity.remision.RegistroHistorialRemision;
import co.com.sura.postgres.repository.agenda.data.CitaData;
import co.com.sura.postgres.repository.agenda.data.CitaRepository;
import co.com.sura.postgres.repository.remision.data.RegistroHistorialRepository;
import co.com.sura.postgres.repository.remision.data.SoporteNutricionalRepository;
import co.com.sura.postgres.repository.remision.data.RemisionRepository;
import co.com.sura.postgres.repository.remision.data.UbicacionRepository;
import co.com.sura.postgres.repository.remision.data.PacienteRepository;
import co.com.sura.postgres.repository.remision.data.DatosAtencionPacienteRepository;
import co.com.sura.postgres.repository.remision.data.RemisionDiagnosticoRepository;
import co.com.sura.postgres.repository.remision.data.TratamientoRepository;
import co.com.sura.postgres.repository.remision.data.CanalizacionRepository;
import co.com.sura.postgres.repository.remision.data.SecrecionRepository;
import co.com.sura.postgres.repository.remision.data.CuracionRepository;
import co.com.sura.postgres.repository.remision.data.FototerapiaRepository;
import co.com.sura.postgres.repository.remision.data.SondajeRepository;
import co.com.sura.postgres.repository.remision.data.TomaMuestraRepository;
import co.com.sura.postgres.repository.remision.data.SoporteNutricionalData;
import co.com.sura.postgres.repository.remision.data.SondajeData;
import co.com.sura.postgres.repository.remision.data.SecrecionData;
import co.com.sura.postgres.repository.remision.data.FototerapiaData;
import co.com.sura.postgres.repository.remision.data.CanalizacionData;
import co.com.sura.postgres.repository.remision.data.TratamientoData;
import co.com.sura.postgres.repository.remision.data.CuracionData;
import co.com.sura.postgres.repository.remision.data.TomaMuestraData;
import co.com.sura.postgres.repository.remision.data.RegistroHistorialRemisionData;
import co.com.sura.postgres.repository.remision.data.RemisionDiagnosticoData;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.util.Comparator;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import static co.com.sura.constantes.Mensajes.REMISION_CITAS_PROGRESO;
import static co.com.sura.constantes.Mensajes.REMISION_NO_EXISTENTE;
import static co.com.sura.entity.remision.TipoNotificacionFarmacia.APLICACION_MEDICAMENTO;
import static co.com.sura.entity.remision.TipoNotificacionFarmacia.SOPORTE_NUTRICIONAL;


@Repository
public class RemisionRepositoryAdapter implements RemisionCrudRepository {

    private final RemisionRepository remisionRepository;
    private final UbicacionRepository ubicacionRepository;
    private final PacienteRepository pacienteRepository;
    private final DatosAtencionPacienteRepository datosAtencionPacienteRepository;
    private final RemisionDiagnosticoRepository remisionDiagnosticoRepository;
    private final CitaRepository citaRepository;
    private final TratamientoRepository tratamientoRepository;
    private final CanalizacionRepository canalizacionRepository;
    private final SecrecionRepository secrecionRepository;
    private final CuracionRepository curacionRepository;
    private final FototerapiaRepository fototerapiaRepository;
    private final SondajeRepository sondajeRepository;
    private final TomaMuestraRepository tomaMuestraRepository;
    private final SoporteNutricionalRepository soporteNutricionalRepository;
    private final RegistroHistorialRepository registroHistorialRemisionRepository;

    @Autowired
    public RemisionRepositoryAdapter(
            RemisionRepository remisionRepository, UbicacionRepository ubicacionRepository,
            PacienteRepository pacienteRepository, DatosAtencionPacienteRepository datosAtencionPacienteRepository,
            RemisionDiagnosticoRepository remisionDiagnosticoRepository, CitaRepository citaRepository,
            TratamientoRepository tratamientoRepository, CanalizacionRepository canalizacionRepository,
            SecrecionRepository secrecionRepository, CuracionRepository curacionRepository,
            FototerapiaRepository fototerapiaRepository, SondajeRepository sondajeRepository,
            TomaMuestraRepository tomaMuestraRepository, SoporteNutricionalRepository soporteNutricionalRepository,
            RegistroHistorialRepository registroHistorialRemisionRepository) {

        this.remisionRepository = remisionRepository;
        this.ubicacionRepository = ubicacionRepository;
        this.pacienteRepository = pacienteRepository;
        this.datosAtencionPacienteRepository = datosAtencionPacienteRepository;
        this.remisionDiagnosticoRepository = remisionDiagnosticoRepository;
        this.citaRepository = citaRepository;
        this.tratamientoRepository = tratamientoRepository;
        this.canalizacionRepository = canalizacionRepository;
        this.secrecionRepository = secrecionRepository;
        this.curacionRepository = curacionRepository;
        this.fototerapiaRepository = fototerapiaRepository;
        this.sondajeRepository = sondajeRepository;
        this.tomaMuestraRepository = tomaMuestraRepository;
        this.soporteNutricionalRepository = soporteNutricionalRepository;
        this.registroHistorialRemisionRepository = registroHistorialRemisionRepository;
    }

    @Override
    public Flux<Remision> consultarRemisiones() {
        return remisionRepository.findAllRemision();
    }

    protected Mono<Void> registrarPacienteRemision(RemisionRequest remisionRequest, boolean esNovedad){
        var ubicacionData = ConverterRemision.extraerUbicacionData(remisionRequest);
        var pacienteData = ConverterRemision.extraerPacienteData(remisionRequest);

        if(esNovedad){
            return Mono.from(ubicacionRepository.save(ubicacionData))
                    .then(Mono.from(pacienteRepository.save(pacienteData)))
                    .then();
        }else {
            return Mono.from(pacienteRepository.existsById(pacienteData.getNumeroIdentificacion()))
                    .flatMap(pacienteExiste ->{
                        if (Boolean.TRUE.equals(pacienteExiste)){
                            return pacienteRepository.save(pacienteData)
                                    .then(Mono.from(ubicacionRepository.save(ubicacionData))).then();
                        }else{
                            return  pacienteRepository.insertpaciente(pacienteData)
                                    .then(Mono.from(ubicacionRepository.insertUbicacion(ubicacionData))).then();
                        }
                    });
        }
    }
    protected  Mono<Void> registrarPlanManejo(RemisionRequest remisionRequest, List<CitaRequest> citasRequest,
                                              NovedadRequest novedadRequest){
        var counter = new AtomicInteger(
                citaRepository.findLastNumberCitaRemision(remisionRequest.getIdRemision())
                        .blockOptional()
                        .orElse(0) + 1);

        if(novedadRequest.getFechaAplicarNovedad() !=null){
            citasRequest.removeIf(
                    citaRequest -> citaRequest.getFechaInicio().isBefore(novedadRequest.getFechaAplicarNovedad()));
        }


        citasRequest.forEach(citaRequest -> {
            String newIdCita = remisionRequest.getIdRemision() + "-" + counter;
            citaRequest.setIdCita(newIdCita);
            counter.getAndIncrement();
        });

        List<CitaData> citasData = ConverterRemision.convertirCitasDataList(citasRequest, remisionRequest);
        List<TratamientoData> tratamientosData = ConverterRemision.extraerTratamientoData(citasRequest);
        List<CanalizacionData> canalizacionesData = ConverterRemision.extraerCanalizacionData(citasRequest);
        List<FototerapiaData> fototerapiaData = ConverterRemision.extraerFototerapiaData(citasRequest);
        List<SecrecionData> secrecionData = ConverterRemision.extraerSecrecionData(citasRequest);
        List<SondajeData> sondajeData = ConverterRemision.extraerSondajeData(citasRequest);
        List<SoporteNutricionalData> soporteNutricionalData = ConverterRemision
                .extraerSoporteNutricionalData(citasRequest);

        List<TomaMuestraData> tomaMuestraData = ConverterRemision.extraerSoporteTomaMuestraData(citasRequest);
        List<CuracionData> curacionesData = ConverterRemision.extraerCuracionData(citasRequest);

        Flux<TratamientoData> tratamientosFlux         = tratamientoRepository.saveAll(tratamientosData);
        Flux<CanalizacionData> canalizacionDataFlux    = canalizacionRepository.saveAll(canalizacionesData);
        Flux<FototerapiaData> fototerapiaDataFlux      = fototerapiaRepository.saveAll(fototerapiaData);
        Flux<SecrecionData> secrecionDataFlux          = secrecionRepository.saveAll(secrecionData);
        Flux<SondajeData> sondajeDataFlux              = sondajeRepository.saveAll(sondajeData);
        Flux<SoporteNutricionalData> soporteNutricionalDataFlux = soporteNutricionalRepository
                .saveAll(soporteNutricionalData);

        Flux<TomaMuestraData> tomaMuestraDataFlux = tomaMuestraRepository.saveAll(tomaMuestraData);
        Flux<CuracionData> curacionDataFlux = curacionRepository.saveAll(curacionesData);

        return  Mono.from(Mono.from(citaRepository.insertMultiplescitas(citasData)))
                .then(tratamientosFlux.collectList())
                .then(canalizacionDataFlux.collectList())
                .then(fototerapiaDataFlux.collectList())
                .then(secrecionDataFlux.collectList())
                .then(sondajeDataFlux.collectList())
                .then(soporteNutricionalDataFlux.collectList())
                .then(tomaMuestraDataFlux.collectList())
                .then(curacionDataFlux.collectList())
                .then();

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
           return  Mono.from(Mono.from(remisionRepository.insertRemision(remisionData)))
                   .then(Mono.from(datosAtencionPacienteRepository.save(datosAtencionPacienteData)))
                   .then(Mono.from(remisionDiagnosticoRepository.insertMultiplesDiagnosticos(diagnosticosData)))
                   .then();
       }
    }

    @Override
    public Mono<Boolean> crearRemisionCita(RemisionRequest remisionRequest, List<CitaRequest> citasRequest) {

        String idRemision = remisionRequest.getIdRemision();
        Mono<Boolean> validarRemision = remisionRepository.existsById(idRemision);
        validarRemision.subscribe();

        if(validarRemision.blockOptional().orElse(false)){
            return Mono.error(new Throwable(Mensajes.REMISION_EXISTENTE.getValue().replace("?",idRemision)));
        }

        String numeroIdentificacionPaciente = remisionRequest.getNumeroIdentificacion();
        return registrarPacienteRemision(remisionRequest,false)
                    .then(registrarDatosRemision(remisionRequest,false))
                    .then(registrarPlanManejo(remisionRequest,citasRequest,new NovedadRequest()))
                    .onErrorMap(throwable -> {
                       Mono<Void>  error = Mono.from(
                           remisionRepository.deleteAllDataRemision(idRemision,numeroIdentificacionPaciente));
                       error.subscribe();
                       return new Exception("Error al crear remision");
                    })
                .then(Mono.just(true));
    }
    @Override
    public Mono<Void> actualizarRemisionPorNovedad(RemisionRequest remisionRequest, List<CitaRequest> citasRequest,
                                                   NovedadRequest novedadRequest) {

        String idRemision = remisionRequest.getIdRemision();
        Mono<Boolean> validarRemision = remisionRepository.existsById(idRemision);
        validarRemision.subscribe();

        if (!validarRemision.blockOptional().orElse(false)) {
            return Mono.error(new Throwable(REMISION_NO_EXISTENTE.getValue().replace("?", idRemision)));
        }

        Mono<RegistroHistorialRemisionData> registroRemision = Mono.from(
            registroHistorialRemisionRepository
                .buildByIdRemisionForUpdate(remisionRequest.getIdRemision(), novedadRequest.getFechaAplicarNovedad()));
        registroRemision.subscribe();

        var registroRemisionData = registroRemision.blockOptional().orElse(new RegistroHistorialRemisionData());
        registroRemisionData.setMotivoNovedad(novedadRequest.getMotivoNovedad());
        registroRemisionData.setFechaAplicacionNovedad(novedadRequest.getFechaAplicarNovedad());

        if (citasRequest == null) {
            registroRemisionData.setCitas(null);
            return Mono.from(registroHistorialRemisionRepository.save(registroRemisionData))
                    .then(Mono.from(citaRepository
                            .deleteCitaDataByIdRemision(idRemision, novedadRequest.getFechaAplicarNovedad())))
                    .then(registrarPacienteRemision(remisionRequest, true))
                    .then(registrarDatosRemision(remisionRequest, true))
                    .then();
        } else {
            return Mono.from(registroHistorialRemisionRepository.save(registroRemisionData))
                    .then(Mono.from(citaRepository
                            .deleteCitaDataByIdRemision(idRemision, novedadRequest.getFechaAplicarNovedad())))
                    .then(registrarPacienteRemision(remisionRequest, true))
                    .then(registrarDatosRemision(remisionRequest, true))
                    .then(registrarPlanManejo(remisionRequest, citasRequest, novedadRequest))
                    .then();
        }

    }

    @Override
    public Mono<Boolean> egresarRemisionById(String idRemision) {
        return remisionRepository.validarEstadosRemisionToEgreso(idRemision)
           .flatMap(exists -> {
               if (Boolean.FALSE.equals(exists)) {
                 return Mono.error(new Throwable(REMISION_NO_EXISTENTE.getValue().replace("?", idRemision)));
               }
               return citaRepository.validarEstadosCitasToEgreso(idRemision)
               .flatMap(valid -> {if (Boolean.TRUE.equals(valid)) {
                   return Mono.error(new Throwable(REMISION_CITAS_PROGRESO.getValue().replace("?", idRemision)));
               }
               return citaRepository.cancelarCitasForEgresoRemision(idRemision)
                       .then(remisionRepository.egresarRemisionById(idRemision));
               });
           }).then(Mono.just(true));
    }

    @Override
    public Mono<DatosAtencionPaciente> consultarDatosAtencionPacienteByIdRemision(String idRemision) {
        return datosAtencionPacienteRepository.findByIdRemision(idRemision)
                .map(ConverterRemision :: convertToDatosAtencionPaciente);
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

    @Override
    public Flux<PacienteTratamientoCita> consultarAllPacienteWithMedicamentosToFarmacia() {
        return pacienteRepository.findAllTratamientosPacientes()
                .map(pacienteTratamientoCita -> {
                            pacienteTratamientoCita.setTipo(APLICACION_MEDICAMENTO.getTipo());
                            return pacienteTratamientoCita;
                        }
                ).mergeWith(pacienteRepository.findAllSoporteNutricionalPacientes()
                        .map(pacienteTratamientoCita -> {
                            pacienteTratamientoCita.setTipo(SOPORTE_NUTRICIONAL.getTipo());
                            return pacienteTratamientoCita;
                })
                ).sort(Comparator.comparing(PacienteTratamientoCita::getNotificado).reversed()
                        .thenComparing(PacienteTratamientoCita::getFechaProgramada).reversed());
    }

    @Override
    public Mono<Void> notificarMedicamentosToFarmacia(List<PacienteTratamientoCita> tratamientoCitasList) {
        return Flux.fromIterable(tratamientoCitasList)
          .flatMap(pacienteTratamientoCita -> {
              Mono<Void> tratamientoUpdate = pacienteTratamientoCita.getIdTratamiento() != null ?
                 tratamientoRepository.updateNotificar(pacienteTratamientoCita.getIdTratamiento()) : Mono.empty();

                Mono<Void> soporteNutricionalUpdate = pacienteTratamientoCita.getIdSoporteNutricional() != null ?
                  soporteNutricionalRepository.updateNotificar(pacienteTratamientoCita.getIdSoporteNutricional()) :
                        Mono.empty();
                    return Mono.when(tratamientoUpdate, soporteNutricionalUpdate);
                })
                .then();
    }

    @Override
    public Flux<RegistroHistorialRemision> consultarHistoricoRemision(String idRemision) {
        return registroHistorialRemisionRepository.findAllByIdRemision(idRemision)
                .map(ConverterRemision ::convertToRegistroHistoriaRemision);
    }

    @Override
    public Mono<RegistroHistorialRemision> consultarDatosRemision(String idRemision) {
        return registroHistorialRemisionRepository.buildByIdRemisionActual(idRemision)
                .map(ConverterRemision::convertToRegistroHistoriaRemision);
    }



}
