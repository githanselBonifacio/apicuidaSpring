package co.com.sura.remision;

import co.com.sura.constantes.Mensajes;
import co.com.sura.genericos.EstadosCita;
import co.com.sura.postgres.agenda.data.CitaData;
import co.com.sura.postgres.agenda.repository.CitaRepository;
import co.com.sura.postgres.remision.adapter.HistorialRemisionAdapter;
import co.com.sura.postgres.remision.adapter.PlanManejoRemisionAdapter;
import co.com.sura.postgres.remision.adapter.RemisionRepositoryAdapter;
import co.com.sura.postgres.remision.data.datospaciente.*;
import co.com.sura.postgres.remision.repository.datospaciente.*;
import co.com.sura.remision.dto.CitaRequest;
import co.com.sura.remision.dto.NovedadRequest;
import co.com.sura.remision.dto.RemisionRequest;
import co.com.sura.remision.entity.Remision;
import co.com.sura.remision.entity.datosremision.DatosAtencionPaciente;
import co.com.sura.remision.entity.datosremision.Paciente;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;

import static co.com.sura.constantes.Mensajes.REMISION_CITAS_PROGRESO;

@ExtendWith(MockitoExtension.class)
class RemisionRepositoryAdapterTest {
    private final String idRemision = "dfkj4540";
    @Mock
    private RemisionRepository remisionRepositoryMock;
    @Mock
    private UbicacionRepository ubicacionRepositoryMock;
    @Mock
    private PacienteRepository pacienteRepositoryMock;
    @Mock
    private PlanManejoRemisionAdapter planManejoRemisionAdapterMock;
    @Mock
    private DatosAtencionPacienteRepository datosAtencionPacienteRepositoryMock;
    @Mock
    private RemisionDiagnosticoRepository remisionDiagnosticoRepositoryMock;
    @Mock
    private CitaRepository citaRepositoryMock;
    @Mock
    private HistorialRemisionAdapter historialRemisionAdapterMock;


    @InjectMocks
    private RemisionRepositoryAdapter remisionRepositoryAdapter;

    @Test
    void consultarRemisiones() {
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
    void crearRemisionCita() {
        //Data request
        RemisionTestData remisionTestData = new RemisionTestData();
        RemisionRequest remisionRequest = remisionTestData.remisionRequest;
        List<CitaRequest> citaRequests = remisionTestData.citasRequest;

        //Data repository
        RemisionData remisionData = remisionTestData.remisionData;
        DatosAtencionPacienteData datosAtencionPacienteData = remisionTestData.datosAtencionPacienteData;
        List<RemisionDiagnosticoData> remisionDiagnosticoDataList = remisionTestData.remisionDiagnosticosData;
        UbicacionData ubicacionData = remisionTestData.ubicacionData;
        PacienteData pacienteData = remisionTestData.pacienteData;

        //mocks remision
        Mockito.when(remisionRepositoryMock.existsById(remisionRequest.getIdRemision()))
                .thenReturn(Mono.just(Boolean.FALSE));

        Mockito.when(remisionRepositoryMock.insertNuevaRemision(remisionRequest.getIdRemision()))
                .thenReturn(Mono.just(Boolean.TRUE));


        Mockito.when(remisionRepositoryMock.save(remisionData))
                .thenReturn(Mono.just(remisionData));

        //mocks paciente
        Mockito.when(pacienteRepositoryMock.existsById(remisionRequest.getNumeroIdentificacion()))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mockito.when(pacienteRepositoryMock.save(pacienteData))
                .thenReturn(Mono.just(pacienteData));

        //mock datos atencion
        Mockito.when(datosAtencionPacienteRepositoryMock.save(datosAtencionPacienteData))
                .thenReturn(Mono.just(datosAtencionPacienteData));

        Mockito.when(remisionDiagnosticoRepositoryMock.saveAll(remisionDiagnosticoDataList))
                .thenReturn(Flux.fromIterable(remisionDiagnosticoDataList));

        Mockito.when(planManejoRemisionAdapterMock.registrarPlanManejo(remisionRequest, citaRequests, 0))
                .thenReturn(Mono.empty());

        Mockito.when(ubicacionRepositoryMock.save(ubicacionData))
                .thenReturn(Mono.just(ubicacionData));


        //crear remision
        Mono<Boolean> response = remisionRepositoryAdapter.crearRemisionCita(
                remisionRequest, citaRequests);

        StepVerifier.create(response)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }

    @Test
    void crearRemisionCitaRemisionExistente() {
        //Data request
        RemisionTestData remisionTestData = new RemisionTestData();
        RemisionRequest remisionRequest = remisionTestData.remisionRequest;
        List<CitaRequest> citaRequests = remisionTestData.citasRequest;


        //mocks remision
        Mockito.when(remisionRepositoryMock.existsById(remisionRequest.getIdRemision()))
                .thenReturn(Mono.just(Boolean.TRUE));


        //crear remision
        Mono<Boolean> response = remisionRepositoryAdapter.crearRemisionCita(
                remisionRequest, citaRequests);

        StepVerifier.create(response)
                .expectErrorMessage(Mensajes.REMISION_EXISTENTE.replace("?", remisionRequest.getIdRemision()))
                .verify();
    }
    @Test
    void crearRemisionCitaPacienteNoExistente() {
        //Data request
        RemisionTestData remisionTestData = new RemisionTestData();
        RemisionRequest remisionRequest = remisionTestData.remisionRequest;
        List<CitaRequest> citaRequests = remisionTestData.citasRequest;

        //Data repository
        RemisionData remisionData = remisionTestData.remisionData;
        DatosAtencionPacienteData datosAtencionPacienteData = remisionTestData.datosAtencionPacienteData;
        List<RemisionDiagnosticoData> remisionDiagnosticoDataList = remisionTestData.remisionDiagnosticosData;
        UbicacionData ubicacionData = remisionTestData.ubicacionData;
        PacienteData pacienteData = remisionTestData.pacienteData;

        //mocks remision
        Mockito.when(remisionRepositoryMock.existsById(remisionRequest.getIdRemision()))
                .thenReturn(Mono.just(Boolean.FALSE));

        Mockito.when(remisionRepositoryMock.insertNuevaRemision(remisionRequest.getIdRemision()))
                .thenReturn(Mono.just(Boolean.TRUE));


        Mockito.when(remisionRepositoryMock.save(remisionData))
                .thenReturn(Mono.just(remisionData));

        //mocks paciente
        Mockito.when(pacienteRepositoryMock.existsById(remisionRequest.getNumeroIdentificacion()))
                .thenReturn(Mono.just(Boolean.FALSE));

        Mockito.when(pacienteRepositoryMock.insertNuevoPaciente(pacienteData.getNumeroIdentificacion()))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mockito.when(pacienteRepositoryMock.save(pacienteData))
                .thenReturn(Mono.just(pacienteData));

        //mock datos atencion
        Mockito.when(datosAtencionPacienteRepositoryMock.save(datosAtencionPacienteData))
                .thenReturn(Mono.just(datosAtencionPacienteData));

        Mockito.when(remisionDiagnosticoRepositoryMock.saveAll(remisionDiagnosticoDataList))
                .thenReturn(Flux.fromIterable(remisionDiagnosticoDataList));

        Mockito.when(planManejoRemisionAdapterMock.registrarPlanManejo(remisionRequest, citaRequests, 0))
                .thenReturn(Mono.empty());

        Mockito.when(ubicacionRepositoryMock.insertNuevaUbicacion(ubicacionData.getIdUbicacion()))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mockito.when(ubicacionRepositoryMock.save(ubicacionData))
                .thenReturn(Mono.just(ubicacionData));


        //crear remision
        Mono<Boolean> response = remisionRepositoryAdapter.crearRemisionCita(
                remisionRequest, citaRequests);

        StepVerifier.create(response)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }

    @Test
    void actualizarRemisionCita() {
        //Data request

        RemisionTestData remisionTestData = new RemisionTestData();
        RemisionRequest remisionRequest = remisionTestData.remisionRequest;
        List<CitaRequest> citaRequests = remisionTestData.citasRequest;

        NovedadRequest novedadRequest = NovedadRequest.builder()
                .motivoNovedad("novedad")
                .fechaAplicarNovedad(LocalDateTime.now().minusDays(1))
                .build();
        String idHistorial = remisionRequest.getIdRemision().concat(String.valueOf(
                novedadRequest.getFechaAplicarNovedad().toEpochSecond(ZoneOffset.UTC)
        ));
        String idCita = remisionRequest.getIdRemision()+"-1";
                //Data repository
        RemisionData remisionData = remisionTestData.remisionData;
        RegistroHistorialRemisionData  registroHistorialData = RegistroHistorialRemisionData
                .builder()
                .id(idHistorial)
                .idRemision(remisionRequest.getIdRemision())
                .fechaRegistro(LocalDateTime.now())
                .build();
        DatosAtencionPacienteData datosAtencionPacienteData = remisionTestData.datosAtencionPacienteData;
        List<RemisionDiagnosticoData> remisionDiagnosticoDataList = remisionTestData.remisionDiagnosticosData;
        UbicacionData ubicacionData = remisionTestData.ubicacionData;
        PacienteData pacienteData = remisionTestData.pacienteData;

        //mocks remision
        Mockito.when(remisionRepositoryMock.existsById(remisionRequest.getIdRemision()))
                .thenReturn(Mono.just(Boolean.TRUE));



        Mockito.when(remisionRepositoryMock.save(remisionData))
                .thenReturn(Mono.just(remisionData));

        //mocks paciente
        Mockito.when(pacienteRepositoryMock.save(pacienteData))
                .thenReturn(Mono.just(pacienteData));

        //citas
        Mockito.when(citaRepositoryMock.findLastNumberIdCita(remisionRequest.getIdRemision()))
                        .thenReturn(Mono.just(0));

       Mockito.when(citaRepositoryMock.deleteById(idCita))
                .thenReturn(Mono.empty());

       Mockito.when(citaRepositoryMock.findAllByIdRemision(remisionRequest.getIdRemision()))
                .thenReturn(Flux.fromIterable(new ArrayList<>(){{add(CitaData.builder()
                        .idCita(remisionRequest.getIdRemision()+"-1")
                        .idEstado(EstadosCita.SIN_AGENDAR.getEstado())
                        .fechaProgramada(LocalDateTime.now())
                        .build());}}));

        Mockito.when(historialRemisionAdapterMock.buildRegistroActualRemision(
                remisionRequest.getIdRemision(),novedadRequest.getFechaAplicarNovedad(),idHistorial))
                .thenReturn(Mono.just( registroHistorialData));

        Mockito.when(historialRemisionAdapterMock.insertRegistro(registroHistorialData))
                .thenReturn(Mono.empty());
        //mock datos atencion
        Mockito.when(datosAtencionPacienteRepositoryMock.updateDatosAtencion(datosAtencionPacienteData))
                .thenReturn(Mono.empty());

       Mockito.when(remisionDiagnosticoRepositoryMock.save(remisionDiagnosticoDataList.get(0)))
                .thenReturn(Mono.just(remisionDiagnosticoDataList.get(0)));

        Mockito.when(ubicacionRepositoryMock.save(ubicacionData))
                .thenReturn(Mono.just(ubicacionData));


        //plan de manejo adapter
       Mockito.when(planManejoRemisionAdapterMock.eliminarPlanManejoByidCita(idCita))
                .thenReturn(Mono.empty());

        Mockito.when(planManejoRemisionAdapterMock.registrarPlanManejo(remisionRequest,citaRequests,0))
                .thenReturn(Mono.empty());

        //actualizar remision
        Mono<Boolean> response = remisionRepositoryAdapter.actualizarRemisionPorNovedad(
                remisionRequest, citaRequests,novedadRequest);


        StepVerifier.create(response)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }
    @Test
    void actualizarRemisionNoExistente() {
        RemisionTestData remisionTestData = new RemisionTestData();
        RemisionRequest remisionRequest = remisionTestData.remisionRequest;
        List<CitaRequest> citaRequests = remisionTestData.citasRequest;
        NovedadRequest novedadRequest = NovedadRequest.builder()
                .motivoNovedad("novedad")
                .fechaAplicarNovedad(LocalDateTime.now())
                .build();

        //mocks remision
        Mockito.when(remisionRepositoryMock.existsById(remisionRequest.getIdRemision()))
                .thenReturn(Mono.just(Boolean.FALSE));

        Mono<Boolean> response = remisionRepositoryAdapter.actualizarRemisionPorNovedad(
                remisionRequest, citaRequests,novedadRequest);

        StepVerifier.create(response)
                .expectErrorMessage(Mensajes.REMISION_NO_EXISTENTE.replace("?", remisionRequest.getIdRemision()))
                .verify();
    }

    @Test
    void egresarRemisionNoExistente(){

        //mocks remision
        Mockito.when(remisionRepositoryMock.validarEstadosRemisionToEgreso(idRemision))
                .thenReturn(Mono.just(Boolean.FALSE));

        Mono<Boolean> response = remisionRepositoryAdapter.egresarRemisionById(idRemision);
        StepVerifier.create(response)
                .expectErrorMessage(Mensajes.REMISION_NO_EXISTENTE.replace("?", idRemision))
                .verify();
    }
    @Test
    void egresarRemisionExistenteEstadosCitasInValidos(){

        //mocks remision
        Mockito.when(remisionRepositoryMock.validarEstadosRemisionToEgreso(idRemision))
                .thenReturn(Mono.just(Boolean.TRUE));



        //mocks citas
        Mockito.when(citaRepositoryMock.validarEstadosToEgreso(
                        idRemision, EstadosCita.CONFIRMADA.getEstado(),EstadosCita.EN_PROGRESO.getEstado()))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Boolean> response = remisionRepositoryAdapter.egresarRemisionById(idRemision);

        StepVerifier.create(response)
                .expectErrorMessage(REMISION_CITAS_PROGRESO.replace("?", idRemision))
                .verify();
    }
    @Test
    void egresarRemisionExistenteEstadosCitasValidos(){


        //mocks remision
        Mockito.when(remisionRepositoryMock.validarEstadosRemisionToEgreso(idRemision))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mockito.when(remisionRepositoryMock.egresarRemisionById(idRemision))
                .thenReturn(Mono.just(Boolean.TRUE));

        //mocks citas
        Mockito.when(citaRepositoryMock.validarEstadosToEgreso(
                idRemision, EstadosCita.CONFIRMADA.getEstado(),EstadosCita.EN_PROGRESO.getEstado()))
                .thenReturn(Mono.just(Boolean.FALSE));

        Mockito.when(citaRepositoryMock.cancelarToEgreso(
                        idRemision,EstadosCita.CANCELADA.getEstado(),
                        EstadosCita.SIN_AGENDAR.getEstado(),EstadosCita.AGENDADA.getEstado()))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Boolean> response = remisionRepositoryAdapter.egresarRemisionById(idRemision);

        StepVerifier.create(response)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarDatosAtencionPaciente(){

        DatosAtencionPacienteData datosAtencionPacientes = DatosAtencionPacienteData.builder()
                .idRemision(idRemision)
                .build();

        Mockito.when(datosAtencionPacienteRepositoryMock.findByIdRemision(idRemision))
                .thenReturn(Mono.just(datosAtencionPacientes));

        Mono<DatosAtencionPaciente> response = remisionRepositoryAdapter
                .consultarDatosAtencionPacienteByIdRemision(idRemision);


        StepVerifier.create(response)
                .expectNextCount(1)
                .expectComplete()
                .verify();
    }
    @Test
    void consultarPacienteFromRemision(){

        PacienteData paciente = PacienteData.builder()
                .numeroIdentificacion("989898")
                .idUbicacion("989898Ubicacion")
                .build();

        UbicacionData ubicacionData = UbicacionData
                .builder()
                .idUbicacion(paciente.getIdUbicacion())
                .build();
        Mockito.when(pacienteRepositoryMock.findPacienteByNumeroIdRemision(idRemision))
                .thenReturn(Mono.just(paciente));

        Mockito.when(ubicacionRepositoryMock.findById(paciente.getIdUbicacion()))
                .thenReturn(Mono.just(ubicacionData));

        Mono<Paciente> response = remisionRepositoryAdapter
                .consultarPacienteFromRemision(idRemision);


        StepVerifier.create(response)
                .expectNextCount(1)
                .expectComplete()
                .verify();
    }
}
