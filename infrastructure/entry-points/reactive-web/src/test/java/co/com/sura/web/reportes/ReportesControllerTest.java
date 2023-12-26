package co.com.sura.web.reportes;

import co.com.sura.constantes.Mensajes;
import co.com.sura.constantes.StatusCode;
import co.com.sura.genericos.Response;
import co.com.sura.reportes.ReportesUseCase;
import co.com.sura.reportes.entity.cancelacioncitas.ReporteCancelacionCitaAnual;
import co.com.sura.reportes.entity.cancelacioncitas.ReporteCancelacionCitaMensual;
import co.com.sura.reportes.entity.turnos.ReporteTurnoAnual;
import co.com.sura.reportes.entity.turnos.ReporteTurnoMensual;
import co.com.sura.reportes.gateway.ReportesRepository;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.security.reactive.ReactiveSecurityAutoConfiguration;
import org.springframework.boot.test.autoconfigure.web.reactive.WebFluxTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.annotation.Import;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.reactive.server.WebTestClient;
import org.springframework.web.util.UriComponentsBuilder;
import reactor.core.publisher.Mono;

import java.util.ArrayList;

@ContextConfiguration(classes = ReportesController.class)
@WebFluxTest(controllers = ReportesController.class,excludeAutoConfiguration = {ReactiveSecurityAutoConfiguration.class})
@ExtendWith(SpringExtension.class)
@ExtendWith(MockitoExtension.class)
@Import(ReportesUseCase.class)
class ReportesControllerTest {

    private final Integer anio = 2023;
    private final String idRegional = "427";
    private final Integer numeroMes = 1;

    @MockBean
    private ReportesRepository reportesRepositoryMock;

    @Autowired
    private WebTestClient webClient;

    @Test
    void consultarReporteAnual(){
        ReporteTurnoAnual reporteTurnoAnual = ReporteTurnoAnual
                .builder()
                .reportes(new ArrayList<>())
                .build();

        Response<ReporteTurnoAnual> respuestaEsperada = Response.<ReporteTurnoAnual>builder()
                .result(reporteTurnoAnual)
                .build();

        Mockito.when(reportesRepositoryMock.consultarReporteAnual(anio,idRegional))
                .thenReturn(Mono.just(reporteTurnoAnual));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/reportes/turno/anual")
                .queryParam("anio", anio)
                .queryParam("idRegional", idRegional);

        Response<ReporteTurnoAnual> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference< Response<ReporteTurnoAnual>>(){})
                .returnResult()
                .getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void consultarReporteAnualError(){

        Mockito.when(reportesRepositoryMock.consultarReporteAnual(anio,idRegional))
                .thenReturn(Mono.error(Exception::new));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/reportes/turno/anual")
                .queryParam("anio", anio)
                .queryParam("idRegional", idRegional);

        Response<?> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());

    }
    @Test
    void consultarReporteMensual(){
        ReporteTurnoMensual reporteTurnoMensual = ReporteTurnoMensual.builder()
                .reportes(new ArrayList<>())
                .build();

        Response<ReporteTurnoMensual> respuestaEsperada = Response.<ReporteTurnoMensual>builder()
                .result(reporteTurnoMensual)
                .build();

        Mockito.when(reportesRepositoryMock.consultarReporteMensual(anio,numeroMes,idRegional))
                .thenReturn(Mono.just(reporteTurnoMensual));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/reportes/turno/mensual")
                .queryParam("anio", anio)
                .queryParam("numeroMes", numeroMes)
                .queryParam("idRegional", idRegional);
        Response<ReporteTurnoMensual> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference< Response<ReporteTurnoMensual>>(){})
                .returnResult()
                .getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void consultarReporteMensualError(){

        Mockito.when(reportesRepositoryMock.consultarReporteMensual(anio,numeroMes,idRegional))
                .thenReturn(Mono.error(Exception::new));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/reportes/turno/mensual")
                .queryParam("anio", anio)
                .queryParam("numeroMes", numeroMes)
                .queryParam("idRegional", idRegional);
        Response<?> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());

    }
    @Test
    void consultarReporteCancelacionCitaAnual(){
        ReporteCancelacionCitaAnual reporteCancelacionCitaAnual = ReporteCancelacionCitaAnual
                .builder()
                .reportes(new ArrayList<>())
                .build();

        Response<ReporteCancelacionCitaAnual> respuestaEsperada = Response.<ReporteCancelacionCitaAnual>builder()
                .result(reporteCancelacionCitaAnual)
                .build();

        Mockito.when(reportesRepositoryMock.consultaReporteCancelacionCitasAnual(anio,idRegional))
                .thenReturn(Mono.just(reporteCancelacionCitaAnual));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/reportes/cancelacionCitas/anual")
                .queryParam("anio", anio)
                .queryParam("idRegional", idRegional);

        Response<ReporteCancelacionCitaAnual> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference< Response<ReporteCancelacionCitaAnual>>(){})
                .returnResult()
                .getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void consultarReporteCancelacionCitaAnualError(){

        Mockito.when(reportesRepositoryMock.consultaReporteCancelacionCitasAnual(anio,idRegional))
                .thenReturn(Mono.error(Exception::new));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/reportes/cancelacionCitas/anual")
                .queryParam("anio", anio)
                .queryParam("idRegional", idRegional);

        Response<?> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());
    }
    @Test
    void consultarReporteCancelacionCitaMensual(){
        ReporteCancelacionCitaMensual reporteCancelacionCitaMensual = ReporteCancelacionCitaMensual.builder()
                .reportes(new ArrayList<>())
                .build();

        Response<ReporteCancelacionCitaMensual> respuestaEsperada = Response.<ReporteCancelacionCitaMensual>builder()
                .result(reporteCancelacionCitaMensual)
                .build();

        Mockito.when(reportesRepositoryMock.consultaReporteCancelacionCitasMensual(anio,numeroMes,idRegional))
                .thenReturn(Mono.just(reporteCancelacionCitaMensual));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/reportes/cancelacionCitas/mensual")
                .queryParam("anio", anio)
                .queryParam("numeroMes", numeroMes)
                .queryParam("idRegional", idRegional);

        Response<ReporteCancelacionCitaMensual> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectStatus().isOk()
                .expectBody(new ParameterizedTypeReference< Response<ReporteCancelacionCitaMensual>>(){})
                .returnResult()
                .getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(respuestaEsperada.getResult(), response.getResult());
    }
    @Test
    void consultarReporteCancelacionCitaMensualError(){

        Mockito.when(reportesRepositoryMock.consultaReporteCancelacionCitasMensual(anio,numeroMes,idRegional))
                .thenReturn(Mono.error(Exception::new));

        UriComponentsBuilder builderUrl = UriComponentsBuilder.fromPath("/reportes/cancelacionCitas/mensual")
                .queryParam("anio", anio)
                .queryParam("numeroMes", numeroMes)
                .queryParam("idRegional", idRegional);

        Response<?> response = webClient.get()
                .uri(builderUrl.build().toUri())
                .exchange()
                .expectBody(Response.class)
                .returnResult().getResponseBody();

        Assertions.assertNotNull(response);
        Assertions.assertEquals(Mensajes.PETICION_FALLIDA, response.getMessage());
        Assertions.assertEquals(StatusCode.STATUS_500, response.getStatus());

    }

}
