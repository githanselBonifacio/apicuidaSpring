package co.com.sura.postgres.repository.remision.adapter;

import co.com.sura.dto.remision.CitaRequest;
import co.com.sura.dto.remision.RemisionRequest;
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

import java.time.Duration;
import java.util.Date;
import java.util.List;


@Repository
public class RemisionRepositoryAdapter implements RemisionCrudRepository {

    private final ConnectionFactory connectionFactory;
    private final DatabaseClient databaseClient;

    public RemisionRepositoryAdapter(ConnectionFactory connectionFactory, DatabaseClient databaseClient) {
        this.connectionFactory = connectionFactory;
        this.databaseClient = databaseClient;
    }

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

    public Mono<Void> crearRemisionCita(RemisionRequest remisionRequest, List<CitaRequest> citasRequest) {
        //Remision
        String idRemision = remisionRequest.getIdRemision();
        String idCiudad = remisionRequest.getCiudad().getIdCiudad();
        UbicacionData ubicacionData = ConverterRemision.extraerUbicacionData(remisionRequest);
        PacienteData pacienteData = ConverterRemision.extraerPacienteData(remisionRequest);
        RemisionData remisionData = ConverterRemision.convertToRemisionRequest(remisionRequest);

        DatosAtencionPacienteData datosAtencionPacienteData = ConverterRemision
                .convertirDatosAtencionPacienteData(remisionRequest.getDatosAtencionPaciente(), idRemision);

        List<RemisionDiagnosticoData> diagnosticosData = ConverterRemision
                .extraerRemisionDiagnosticoData(remisionRequest.getDiagnosticos(), idRemision);

        //citas
        List<CitaData> citasData = ConverterRemision.convertirCitasDataList(citasRequest, idRemision,idCiudad);

        List<TratamientoData> tratamientosData = ConverterRemision.extraerTratamientoData(citasRequest);

        List<CanalizacionData> canalizacionesData = ConverterRemision.extraerCanalizacionData(citasRequest);

        List<FototerapiaData> fototerapiaData = ConverterRemision.extraerFototerapiaData(citasRequest);

        List<SecrecionData> secrecionData = ConverterRemision.extraerSecrecionData(citasRequest);

        List<SondajeData> sondajeData = ConverterRemision.extraerSondajeData(citasRequest);

        List<SoporteNutricionalData> soporteNutricionalData = ConverterRemision
                .extraerSoporteNutricionalData(citasRequest);

        List<TomaMuestraData> tomaMuestraData = ConverterRemision.extraerSoporteTomaMuestraData(citasRequest);

        List<CuracionData> curacionesData = ConverterRemision.extraerCuracionData(citasRequest);

        try {
            Flux<TratamientoData> tratamientosFlux = tratamientoRepository.saveAll(tratamientosData);
            Flux<RemisionDiagnosticoData> diagnosticosFlux = remisionDiagnosticoRepository.saveAll(diagnosticosData);
            Flux<CanalizacionData> canalizacionDataFlux = canalizacionRepository.saveAll(canalizacionesData);
            Flux<FototerapiaData> fototerapiaDataFlux = fototerapiaRepository.saveAll(fototerapiaData);
            Flux<SecrecionData> secrecionDataFlux = secrecionRepository.saveAll(secrecionData);
            Flux<SondajeData> sondajeDataFlux = sondajeRepository.saveAll(sondajeData);

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
                    .then();

        } catch (Exception e) {
            Mono<Void> deleteData = Mono.from(ubicacionRepository.delete(ubicacionData))
                    .then(Mono.from(pacienteRepository.delete(pacienteData)))
                    .then(Mono.from(remisionRepository.delete(remisionData)))
                    .then(Mono.from(citaRepository.deleteAll(citasData)))
                    .then(Mono.from(remisionDiagnosticoRepository.deleteAll(diagnosticosData)))
                    .then(Mono.from(datosAtencionPacienteRepository.delete(datosAtencionPacienteData)));

            return deleteData.then(Mono.error(e));
        }
    }

}
