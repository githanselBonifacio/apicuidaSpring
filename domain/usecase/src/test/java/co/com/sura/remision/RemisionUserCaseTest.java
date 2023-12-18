package co.com.sura.remision;


import co.com.sura.remision.dto.CitaRequest;
import co.com.sura.remision.dto.NovedadRequest;
import co.com.sura.remision.dto.RemisionRequest;
import co.com.sura.remision.entity.Remision;
import co.com.sura.remision.entity.datosremision.DatosAtencionPaciente;
import co.com.sura.remision.entity.datosremision.Paciente;
import co.com.sura.remision.entity.historial.RegistroHistorialRemision;
import co.com.sura.remision.gateway.HistorialRemisionRepository;
import co.com.sura.remision.gateway.RemisionCrudRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
class RemisionUserCaseTest {
    private final String idRemision = "asdhkjas";
    @Mock
    private RemisionCrudRepository  remisionCrudRepositoryCrud;

    @Mock
    private HistorialRemisionRepository historialRemisionRepositoryMock;
    @InjectMocks
    private RemisionUseCase remisionUseCaseMock;

    @Test
    void consultarRemisiones(){
        List<Remision> remisiones = new ArrayList<>();
        remisiones.add(Remision.builder().build());
        remisiones.add(Remision.builder().build());

        Mockito.when(remisionCrudRepositoryCrud.consultarRemisiones())
                .thenReturn(Flux.fromIterable(remisiones));

        Flux<Remision> consultaRemisiones = remisionUseCaseMock
                .consultarRemisiones();

        StepVerifier.create(consultaRemisiones)
                .expectNextCount(2)
                .expectComplete()
                .verify();

    }

    @Test
    void crearRemisionCitas(){
        RemisionRequest remisionRequest = RemisionRequest.builder().build();
        List<CitaRequest> citaRequests = new ArrayList<>();
        citaRequests.add(CitaRequest.builder().build());
        citaRequests.add(CitaRequest.builder().build());

        Mockito.when(remisionCrudRepositoryCrud.crearRemisionCita(remisionRequest,citaRequests))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Boolean> respuesta = remisionUseCaseMock.crearRemisionCitas(remisionRequest,citaRequests);

        StepVerifier.create(respuesta)
               .expectNext(Boolean.TRUE)
               .expectComplete()
               .verify();
    }

    @Test
    void actualizarRemisionPorNovedad(){
        RemisionRequest remisionRequest = RemisionRequest.builder().build();
        List<CitaRequest> citaRequests = new ArrayList<>();
        citaRequests.add(CitaRequest.builder().build());
        citaRequests.add(CitaRequest.builder().build());
        NovedadRequest novedadRequest = NovedadRequest.builder().build();

        Mockito.when(remisionCrudRepositoryCrud.actualizarRemisionPorNovedad(
                remisionRequest,citaRequests,novedadRequest))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Boolean> respuesta = remisionUseCaseMock.actualizarRemisionPorNovedad(
                remisionRequest,citaRequests,novedadRequest);

        StepVerifier.create(respuesta)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarHistorialRemisionById(){
        List<RegistroHistorialRemision> registroHistorialRemision = new ArrayList<>();
        registroHistorialRemision.add(RegistroHistorialRemision.builder().build());
        registroHistorialRemision.add(RegistroHistorialRemision.builder().build());

        Mockito.when(historialRemisionRepositoryMock.consultarHistoricoRemision(idRemision))
                .thenReturn(Flux.fromIterable(registroHistorialRemision));

        Flux<RegistroHistorialRemision> consultaHistorial = remisionUseCaseMock
                .consultarHistorialRemisionById(idRemision);

        StepVerifier.create(consultaHistorial)
                .expectNextCount(2)
                .expectComplete()
                .verify();

    }

    @Test
    void consultarDataActualRemision(){
        RegistroHistorialRemision registroHistorialRemision = RegistroHistorialRemision.builder().build();

        Mockito.when(historialRemisionRepositoryMock.consultarDatosRemision(idRemision))
                .thenReturn(Mono.just(registroHistorialRemision));

        Mono<RegistroHistorialRemision> consultaRegistroHistorial = remisionUseCaseMock
                .consultarDataActualRemision(idRemision);

        StepVerifier.create(consultaRegistroHistorial)
                .expectNext(registroHistorialRemision)
                .expectComplete()
                .verify();
    }
    @Test
    void egresarRemisionById(){

        Mockito.when(remisionCrudRepositoryCrud.egresarRemisionById(idRemision))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Boolean> seEgresoRemision = remisionUseCaseMock
                .egresarRemisionById(idRemision);

        StepVerifier.create(seEgresoRemision)
                .expectNext(Boolean.TRUE)
                .expectComplete()
                .verify();

    }

    @Test
    void consultarDatosAtencionPacienteByRemision(){
        DatosAtencionPaciente datosAtencionPaciente = DatosAtencionPaciente.builder().build();

        Mockito.when(remisionCrudRepositoryCrud.consultarDatosAtencionPacienteByIdRemision(idRemision))
                .thenReturn(Mono.just(datosAtencionPaciente));

        Mono<DatosAtencionPaciente> datosAtencionPacienteConsulta = remisionUseCaseMock
                .consultarDatosAtencionPacienteByRemision(idRemision);

        StepVerifier.create(datosAtencionPacienteConsulta)
                .expectNext(datosAtencionPaciente)
                .expectComplete()
                .verify();
    }

    @Test
    void consultarPacienteFromRemision(){
        Paciente paciente = Paciente.builder().build();

        Mockito.when(remisionCrudRepositoryCrud.consultarPacienteFromRemision(idRemision))
                .thenReturn(Mono.just(paciente));

        Mono<Paciente> pacienteConsultado = remisionUseCaseMock
                .consultarPacienteFromRemision(idRemision);

        StepVerifier.create(pacienteConsultado)
                .expectNext(paciente)
                .expectComplete()
                .verify();
    }
}
