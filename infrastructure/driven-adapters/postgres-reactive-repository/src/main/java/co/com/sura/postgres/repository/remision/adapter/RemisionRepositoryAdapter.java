package co.com.sura.postgres.repository.remision.adapter;

import co.com.sura.dto.remision.CitaRequest;
import co.com.sura.dto.remision.RemisionRequest;
import co.com.sura.entity.agenda.PacienteTratamientoCita;
import co.com.sura.entity.remision.*;
import co.com.sura.postgres.repository.agenda.data.CitaData;
import co.com.sura.postgres.repository.agenda.data.CitaRepository;
import co.com.sura.postgres.repository.remision.data.*;
import io.r2dbc.spi.ConnectionFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.r2dbc.core.DatabaseClient;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.util.Comparator;
import java.util.List;

import static co.com.sura.entity.remision.TipoNotificacionFarmacia.APLICACION_MEDICAMENTO;
import static co.com.sura.entity.remision.TipoNotificacionFarmacia.SOPORTE_NUTRICIONAL;


@Repository
public class RemisionRepositoryAdapter implements RemisionCrudRepository {

    @Autowired
    private RemisionRepository remisionRepository;

    @Autowired
    private UbicacionRepository ubicacionRepository;

    @Autowired
    private PacienteRepository pacienteRepository;

    @Autowired
    private DatosAtencionPacienteRepository datosAtencionPacienteRepository;

    @Autowired
    private RemisionDiagnosticoRepository remisionDiagnosticoRepository;

    @Autowired
    private CitaRepository citaRepository;

    @Autowired
    private  TratamientoRepository tratamientoRepository;

    @Autowired
    private CanalizacionRepository canalizacionRepository;

    @Autowired
    private SecrecionRepository secrecionRepository;

    @Autowired
    private CuracionRepository curacionRepository;

    @Autowired
    private FototerapiaRepository fototerapiaRepository;

    @Autowired
    private SondajeRepository sondajeRepository;

    @Autowired
    private TomaMuestraRepository tomaMuestraRepository;

    @Autowired
    private SoporteNutricionalRepository soporteNutricionalRepository;

    private final ConnectionFactory connectionFactory;
    private final DatabaseClient databaseClient;

    public RemisionRepositoryAdapter(ConnectionFactory connectionFactory, DatabaseClient databaseClient) {
        this.connectionFactory = connectionFactory;
        this.databaseClient = databaseClient;
    }

    public Mono<Void> crearRemisionCita(RemisionRequest remisionRequest, List<CitaRequest> citasRequest) {
        //Remision
        String idRemision = remisionRequest.getIdRemision();
        Mono<Boolean> validarRemision = remisionRepository.existsById(idRemision);
        validarRemision.subscribe();

        if(validarRemision.blockOptional().orElse(false)){
            return Mono.error(new Throwable("Ya existe una remision con el id "+idRemision));
        }
        var ubicacionData = ConverterRemision.extraerUbicacionData(remisionRequest);
        var pacienteData = ConverterRemision.extraerPacienteData(remisionRequest);
        var remisionData = ConverterRemision.convertToRemisionRequest(remisionRequest);
        var datosAtencionPacienteData = ConverterRemision
                .convertirDatosAtencionPacienteData(remisionRequest.getDatosAtencionPaciente(), idRemision);

        List<RemisionDiagnosticoData> diagnosticosData = ConverterRemision
                .extraerRemisionDiagnosticoData(remisionRequest.getDiagnosticos(), idRemision);

        //citas
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
        Flux<RemisionDiagnosticoData> diagnosticosFlux = remisionDiagnosticoRepository.saveAll(diagnosticosData);
        Flux<CanalizacionData> canalizacionDataFlux    = canalizacionRepository.saveAll(canalizacionesData);
        Flux<FototerapiaData> fototerapiaDataFlux      = fototerapiaRepository.saveAll(fototerapiaData);
        Flux<SecrecionData> secrecionDataFlux          = secrecionRepository.saveAll(secrecionData);
        Flux<SondajeData> sondajeDataFlux              = sondajeRepository.saveAll(sondajeData);

        Flux<SoporteNutricionalData> soporteNutricionalDataFlux = soporteNutricionalRepository
                    .saveAll(soporteNutricionalData);

        Flux<TomaMuestraData> tomaMuestraDataFlux = tomaMuestraRepository.saveAll(tomaMuestraData);
        Flux<CuracionData> curacionDataFlux = curacionRepository.saveAll(curacionesData);

        return Mono.from(ubicacionRepository.insertUbicacion(ubicacionData))
                    .then(Mono.from(pacienteRepository.insertpaciente(pacienteData)))

                    .then(Mono.from(remisionRepository.insertRemision(remisionData)))

                    .then(Mono.from(citaRepository.insertMultiplescitas(citasData)))

                    .then(Mono.from(datosAtencionPacienteRepository
                            .save(datosAtencionPacienteData)))

                    .then(tratamientosFlux.collectList())
                    .then(diagnosticosFlux.collectList())
                    .then(canalizacionDataFlux.collectList())
                    .then(fototerapiaDataFlux.collectList())
                    .then(secrecionDataFlux.collectList())
                    .then(sondajeDataFlux.collectList())
                    .then(soporteNutricionalDataFlux.collectList())
                    .then(tomaMuestraDataFlux.collectList())
                    .then(curacionDataFlux.collectList())
                    .then()
                    .onErrorMap(throwable -> {
                       Mono<Void>  error = Mono.from(
                           remisionRepository.deleteAllDataRemision(idRemision,pacienteData.getNumeroIdentificacion()));
                       error.subscribe();
                       return new Exception("Error al crear remision");
                    });
    }

    @Override
    public Mono<DatosAtencionPaciente> consultarDatosAtencionPacienteByIdRemision(String idRemision) {
        return datosAtencionPacienteRepository.findAllByIdRemision(idRemision)
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


}
