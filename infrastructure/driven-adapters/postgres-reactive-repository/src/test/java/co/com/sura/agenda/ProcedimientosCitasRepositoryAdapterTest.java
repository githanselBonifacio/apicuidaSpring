package co.com.sura.agenda;

import co.com.sura.postgres.agenda.adapter.ProcedimientosCitasAdapter;
import co.com.sura.postgres.remision.data.procedimientos.*;
import co.com.sura.postgres.remision.repository.procedimientos.*;
import co.com.sura.remision.entity.datosremision.Medicamento;
import co.com.sura.remision.entity.procedimientos.*;
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


@ExtendWith(MockitoExtension.class)
 class ProcedimientosCitasRepositoryAdapterTest {
    @Mock
    private  CuracionRepository curacionRepositoryMock;
    @Mock
    private  CanalizacionRepository canalizacionRepositoryMock;
    @Mock
    private  FototerapiaRepository fototerapiaRepositoryMock;
    @Mock
    private  SecrecionRepository secrecionRepositoryMock;
    @Mock
    private  SondajeRepository sondajeRepositoryMock;
    @Mock
    private  SoporteNutricionalRepository soporteNutricionalRepositoryMock;
    @Mock
    private  TomaMuestraRepository tomaMuestraRepositoryMock;

    @InjectMocks
    private ProcedimientosCitasAdapter procedimientosCitasAdapter;

    @Test
    void consultarProcedimientosByIdCita(){
        Procedimientos procedimientos = Procedimientos.builder()
                .canalizaciones(new ArrayList<>(){{add(Canalizacion.builder().build());}})
                .fototerapias(new ArrayList<>(){{add(Fototerapia.builder().build());}})
                .secreciones(new ArrayList<>(){{add(Secrecion.builder().build());}})
                .curaciones(new ArrayList<>(){{add(Curacion.builder().build());}})
                .sondajes(new ArrayList<>(){{add(Sondaje.builder().build());}})
                .soporteNutricionales(new ArrayList<>(){{add(SoporteNutricional.builder()
                        .medicamento(Medicamento.builder().build())
                        .noPBS(Boolean.TRUE)
                        .build());}})
                .tomaMuestras(new ArrayList<>(){{add(TomaMuestra.builder().build());}})
                .build();

        String idCita = "ds546-2";
        Mockito.when(curacionRepositoryMock.findByIdCita(idCita))
                .thenReturn(Flux.fromIterable(new ArrayList<>(){{add(CuracionData.builder().build());}}));

        Mockito.when(canalizacionRepositoryMock.findByIdCita(idCita))
                .thenReturn(Flux.fromIterable(new ArrayList<>(){{add(CanalizacionData.builder().build());}}));

        Mockito.when(fototerapiaRepositoryMock.findByIdCita(idCita))
                .thenReturn(Flux.fromIterable(new ArrayList<>(){{add(FototerapiaData.builder().build());}}));

        Mockito.when(secrecionRepositoryMock.findByIdCita(idCita))
                .thenReturn(Flux.fromIterable(new ArrayList<>(){{add(SecrecionData.builder().build());}}));

        Mockito.when(sondajeRepositoryMock.findByIdCita(idCita))
                .thenReturn(Flux.fromIterable(new ArrayList<>(){{add(SondajeData.builder().build());}}));

        Mockito.when(tomaMuestraRepositoryMock.findByIdCita(idCita))
                .thenReturn(Flux.fromIterable(new ArrayList<>(){{add(TomaMuestraData.builder().build());}}));

        Mockito.when(soporteNutricionalRepositoryMock.findByIdCita(idCita))
                .thenReturn(Flux.fromIterable(new ArrayList<>(){{add(SoporteNutricionalData.builder()
                        .noPBS(Boolean.TRUE)
                        .build());}}));

        Mono<Procedimientos> response = procedimientosCitasAdapter.consultarProcedimientosByIdCita(idCita);

        StepVerifier.create(response)
                .expectNext(procedimientos)
                .expectComplete()
                .verify();
    }
}
