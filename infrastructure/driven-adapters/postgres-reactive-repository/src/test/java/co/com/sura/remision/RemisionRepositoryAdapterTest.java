package co.com.sura.remision;

import co.com.sura.RemisionTestData;
import co.com.sura.postgres.agenda.repository.CitaRepository;
import co.com.sura.postgres.remision.adapter.HistorialRemisionAdapter;
import co.com.sura.postgres.remision.adapter.PlanManejoRemisionAdapter;
import co.com.sura.postgres.remision.adapter.RemisionRepositoryAdapter;
import co.com.sura.postgres.remision.data.datospaciente.*;
import co.com.sura.postgres.remision.repository.datospaciente.*;
import co.com.sura.remision.dto.CitaRequest;
import co.com.sura.remision.dto.RemisionRequest;
import co.com.sura.remision.entity.Remision;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
class RemisionRepositoryAdapterTest {

    private  final String idRemision = "dfkj4540";
    private  final String idCita = "dfkj4540-2";
    @Mock
    private  RemisionRepository remisionRepositoryMock;
    @Mock
    private  UbicacionRepository ubicacionRepositoryMock;
    @Mock
    private  PacienteRepository pacienteRepositoryMock;
    @Mock
    private  PlanManejoRemisionAdapter planManejoRemisionAdapterMock;
    @Mock
    private  DatosAtencionPacienteRepository datosAtencionPacienteRepositoryMock;
    @Mock
    private  RemisionDiagnosticoRepository remisionDiagnosticoRepositoryMock;
    @Mock
    private  CitaRepository citaRepositoryMock;
    @Mock
    private  HistorialRemisionAdapter historialRemisionAdapterMock;

    private RemisionRepositoryAdapter remisionRepositoryAdapter;

    @BeforeEach
    void  setUp(){
        remisionRepositoryAdapter = new RemisionRepositoryAdapter(
             remisionRepositoryMock,
             ubicacionRepositoryMock,
             pacienteRepositoryMock,
             planManejoRemisionAdapterMock,
             datosAtencionPacienteRepositoryMock,
             remisionDiagnosticoRepositoryMock,
             citaRepositoryMock,
             historialRemisionAdapterMock
        );
    }

    @Test
    void consultarRemisiones(){
        List<Remision> remisionesData = new ArrayList<>();
        remisionesData.add(Remision.builder()
                .idRemision(idRemision)
                .build());

        Mockito.when(remisionRepositoryMock.findAllRemision())
                .thenReturn(Flux.fromIterable(remisionesData));

        Flux<Remision> response = remisionRepositoryAdapter.consultarRemisiones();


        StepVerifier.create(response)
                .expectNextCount(1)
                .expectComplete()
                .verify();
    }

    @Test
    void crearRemisionCita(){
        RemisionTestData remisionTestData = new RemisionTestData();
        RemisionRequest remisionRequest = remisionTestData.remisionRequest;
        List<CitaRequest> citaRequests = remisionTestData.citasRequest;
        RemisionData remisionData = remisionTestData.remisionData;
        DatosAtencionPacienteData datosAtencionPacienteData = remisionTestData.datosAtencionPacienteData;
        List<RemisionDiagnosticoData> remisionDiagnosticoDataList = remisionTestData.remisionDiagnosticosData;
        UbicacionData ubicacionData = remisionTestData.ubicacionData;
        PacienteData pacienteData = remisionTestData.pacienteData;

        Mockito.when(remisionRepositoryMock.existsById(remisionRequest.getIdRemision()))
                .thenReturn(Mono.just(Boolean.FALSE));

        Mockito.when(pacienteRepositoryMock.existsById(remisionRequest.getNumeroIdentificacion()))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mockito.when(remisionRepositoryMock.insertNuevaRemision(remisionRequest.getIdRemision()))
                .thenReturn(Mono.just(Boolean.TRUE));


        Mockito.when(remisionRepositoryMock.save(remisionData))
                .thenReturn(Mono.just(remisionData));

        Mockito.when(datosAtencionPacienteRepositoryMock.save(datosAtencionPacienteData))
                .thenReturn(Mono.just(datosAtencionPacienteData));

        Mockito.when(remisionDiagnosticoRepositoryMock.insertMultiplesDiagnosticos(remisionDiagnosticoDataList))
                .thenReturn(Mono.empty());
        Mockito.when(planManejoRemisionAdapterMock.registrarPlanManejo(remisionRequest,citaRequests,0))
                .thenReturn(Mono.empty());

        Mockito.when(ubicacionRepositoryMock.save(ubicacionData))
                .thenReturn(Mono.just(ubicacionData));

        Mockito.when(pacienteRepositoryMock.save(pacienteData))
                .thenReturn(Mono.just(pacienteData));


        Mono<Boolean> response = remisionRepositoryAdapter.crearRemisionCita(
                remisionRequest,citaRequests);


        StepVerifier.create(response)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }
}
