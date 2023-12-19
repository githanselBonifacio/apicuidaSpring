package co.com.sura.farmacia;

import co.com.sura.agenda.entity.PacienteTratamientoCita;
import co.com.sura.postgres.farmacia.adapter.FarmaciaAdapter;
import co.com.sura.postgres.remision.repository.datospaciente.PacienteRepository;
import co.com.sura.postgres.remision.repository.procedimientos.SoporteNutricionalRepository;
import co.com.sura.postgres.remision.repository.tratamientos.TratamientoRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
 class FarmaciaAdapterTest {

    @Mock
    private  PacienteRepository pacienteRepositoryMock;
    @Mock
    private  SoporteNutricionalRepository soporteNutricionalRepositoryMock;
    @Mock
    private  TratamientoRepository tratamientoRepositoryMock;

    private FarmaciaAdapter farmaciaAdapter;

    @BeforeEach
    void setUp(){
        farmaciaAdapter = new FarmaciaAdapter(
                pacienteRepositoryMock,soporteNutricionalRepositoryMock,tratamientoRepositoryMock
        );
    }

    @Test
    void consultarAllPacienteWithMedicamentosToFarmacia(){
        String numeroIdentificacion = "989898";
        List<PacienteTratamientoCita> pacienteTratamientoCitas = new ArrayList<>();
        pacienteTratamientoCitas.add(PacienteTratamientoCita.builder()
                .numeroIdentificacion(numeroIdentificacion)
                        .fechaProgramada(LocalDateTime.now())
                .notificado(Boolean.FALSE)
                .build());

        Mockito.when(pacienteRepositoryMock.findAllTratamientosPacientes())
                .thenReturn(Flux.fromIterable(pacienteTratamientoCitas));

        Mockito.when(pacienteRepositoryMock.findAllSoporteNutricionalPacientes())
                .thenReturn(Flux.fromIterable(pacienteTratamientoCitas));

        Flux<PacienteTratamientoCita> response = farmaciaAdapter.consultarAllPacienteWithMedicamentosToFarmacia();


        StepVerifier.create(response)
                .expectNextCount(1)
                .expectNext(pacienteTratamientoCitas.get(0))
                .expectComplete()
                .verify();
    }
    @Test
    void consultarAllPacienteWithMedicamentosToFarmaciaByFilter(){
        String numeroIdentificacion = "989898";
        LocalDate fecha = LocalDate.now();
        Integer idHorarioTurno =1;
        String idRegional = "427";

        List<PacienteTratamientoCita> pacienteTratamientoCitas = new ArrayList<>();
        pacienteTratamientoCitas.add(PacienteTratamientoCita.builder()
                .numeroIdentificacion(numeroIdentificacion)
                .fechaProgramada(LocalDateTime.now())
                .notificado(Boolean.FALSE)
                .build());

        Mockito.when(pacienteRepositoryMock.findAllTratamientosPacientesByTurnoRegionalHorario(
                fecha,idHorarioTurno,idRegional))
                .thenReturn(Flux.fromIterable(pacienteTratamientoCitas));

        Mockito.when(pacienteRepositoryMock.findAllSoporteNutricionalPacientesByTurnoRegionalHorario(
                        fecha,idHorarioTurno,idRegional))
                .thenReturn(Flux.fromIterable(pacienteTratamientoCitas));

        Flux<PacienteTratamientoCita> response = farmaciaAdapter
                .consultarAllPacienteWithMedicamentosToFarmaciaByFilter( fecha,idHorarioTurno,idRegional);


        StepVerifier.create(response)
                .expectNextCount(1)
                .expectNext(pacienteTratamientoCitas.get(0))
                .expectComplete()
                .verify();
    }

    @Test
    void notificarMedicamentosToFarmacia(){
        List<PacienteTratamientoCita> pacienteTratamientoCitas = new ArrayList<>();
        pacienteTratamientoCitas.add(PacienteTratamientoCita.builder()
                 .idTratamiento(1)
                .fechaProgramada(LocalDateTime.now())
                .notificado(Boolean.FALSE)
                .build());

        pacienteTratamientoCitas.add(PacienteTratamientoCita.builder()
                .idSoporteNutricional(1)
                .fechaProgramada(LocalDateTime.now())
                .notificado(Boolean.FALSE)
                .build());

        Mono<Boolean> response = farmaciaAdapter.notificarMedicamentosToFarmacia(pacienteTratamientoCitas);


        StepVerifier.create(response)
                .expectNextMatches(b->b.equals(Boolean.TRUE))
                .expectComplete()
                .verify();
    }
}
