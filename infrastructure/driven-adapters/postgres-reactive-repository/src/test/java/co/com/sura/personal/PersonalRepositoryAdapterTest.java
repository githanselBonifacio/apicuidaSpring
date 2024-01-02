package co.com.sura.personal;

import co.com.sura.constantes.Mensajes;
import co.com.sura.moviles.entity.Movil;
import co.com.sura.personal.entity.Conductor;
import co.com.sura.personal.entity.Profesional;
import co.com.sura.postgres.personal.adapter.PersonalRepositoryAdapter;
import co.com.sura.postgres.personal.data.ConductorData;
import co.com.sura.postgres.personal.data.MovilData;
import co.com.sura.postgres.personal.data.ProfesionalData;
import co.com.sura.postgres.personal.repository.ConductorRepository;
import co.com.sura.postgres.personal.repository.MovilRepository;
import co.com.sura.postgres.personal.repository.ProfesionalRepository;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

@ExtendWith(MockitoExtension.class)
class PersonalRepositoryAdapterTest {
    private final String idRegional = "427";
    private final String idProfesional = "989898";
    private final LocalDate fechaTurno = LocalDate.now();
    private ProfesionalData profesionalData;
    private Profesional profesional;
    private ConductorData conductorData;
    private Conductor conductor;
    private MovilData movilData;
    private Movil movil;
    @Mock
    private  ProfesionalRepository profesionalRepositoryMock;
    @Mock
    private  MovilRepository movilRepositoryMock;
    @Mock
    private  ConductorRepository conductorRepositoryMock;

    @InjectMocks
    private PersonalRepositoryAdapter personalRepositoryAdapter;

    @BeforeEach
    void setUpData(){
        //profesionales
         this.profesionalData =ProfesionalData.builder()
                .numeroIdentificacion(idProfesional)
                .idRegional(idRegional)
                .build();

         this.profesional =Profesional.builder()
                .numeroIdentificacion(idProfesional)
                .idRegional(idRegional)
                .build();

         //conductores
        this.conductorData = ConductorData.builder()
                .numeroIdentificacion(idProfesional)
                .idRegional(idRegional)
                .build();

        this.conductor = Conductor.builder()
                .numeroIdentificacion(idProfesional)
                .idRegional(idRegional)
                .build();

        //moviles
        String matricula = "gdf-451";
        this.movilData = MovilData.builder()
                .idRegional(idRegional)
                .matricula(matricula)
                .build();

        this.movil = Movil.builder()
                .idRegional(idRegional)
                .matricula(matricula)
                .build();
    }
    //profesionales
    @Test
    void consultarProfesionales(){
        List<ProfesionalData> profesionalesData = new ArrayList<>(){{add(profesionalData);}};

        Mockito.when(profesionalRepositoryMock.findAll())
                .thenReturn(Flux.fromIterable(profesionalesData));

        Flux<Profesional> response = personalRepositoryAdapter.consultarProfesionales();

        StepVerifier.create(response)
                .expectNext(this.profesional)
                .verifyComplete();
    }
    @Test
    void consultarProfesionalesByRegional(){

        List<ProfesionalData> profesionalesData = new ArrayList<>(){{add(profesionalData);}};

        Mockito.when(profesionalRepositoryMock.findByIdRegional(idRegional))
                .thenReturn(Flux.fromIterable(profesionalesData));

        Flux<Profesional> response = personalRepositoryAdapter.consultarProfesionalesByIdRegional(idRegional);

        StepVerifier.create(response)
                .expectNext(Profesional.builder()
                        .numeroIdentificacion(idProfesional)
                        .idRegional(idRegional)
                        .build())
                .verifyComplete();
    }
    @Test
    void consultarProfesionalByTurnoRegional(){

        List<ProfesionalData> profesionalesData = new ArrayList<>(){{add(profesionalData);}};

        Mockito.when(profesionalRepositoryMock.findByTurnoRegional(fechaTurno,idRegional))
                .thenReturn(Flux.fromIterable(profesionalesData));

        Flux<Profesional> response = personalRepositoryAdapter
                .consultarProfesionalByTurnoRegional(fechaTurno,idRegional);

        StepVerifier.create(response)
                .expectNext(Profesional.builder()
                        .numeroIdentificacion(idProfesional)
                        .idRegional(idRegional)
                        .build())
                .verifyComplete();
    }
    @Test
    void consultarProfesionalFromTurnoRegional(){

        List<ProfesionalData> profesionalesData = new ArrayList<>(){{add(profesionalData);}};

        Integer idHorarioTurno = 1;
        Mockito.when(profesionalRepositoryMock.findFromTurnoRegional(fechaTurno,idRegional, idHorarioTurno))
                .thenReturn(Flux.fromIterable(profesionalesData));

        Flux<Profesional> response = personalRepositoryAdapter
                .consultarProfesionalFromTurnoRegional(fechaTurno,idRegional, idHorarioTurno);

        StepVerifier.create(response)
                .expectNext(Profesional.builder()
                        .numeroIdentificacion(idProfesional)
                        .idRegional(idRegional)
                        .build())
                .verifyComplete();
    }
    @Test
    void crearProfesionalValidoExistenteError(){

        Mockito.when(profesionalRepositoryMock.existsById(this.profesionalData.getNumeroIdentificacion()))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Profesional> response = personalRepositoryAdapter
                .crearProfesional(this.profesional)
                .thenReturn(this.profesional);

        StepVerifier.create(response)
                .expectErrorMessage(Mensajes.PROFESIONAL_YA_EXISTE)
                .verify();

    }
    @Test
    void crearProfesionalValido(){

        Mockito.when(profesionalRepositoryMock.existsById(this.profesionalData.getNumeroIdentificacion()))
                .thenReturn(Mono.just(Boolean.FALSE));

        Mockito.when(profesionalRepositoryMock.insertProfesional(this.profesionalData.getNumeroIdentificacion()))
                .thenReturn(Mono.empty());

        Mockito.when(profesionalRepositoryMock.save(this.profesionalData))
                .thenReturn(Mono.just(this.profesionalData));

        Mockito.when(profesionalRepositoryMock.findById(this.profesionalData.getNumeroIdentificacion()))
                .thenReturn(Mono.just(this.profesionalData));

        Mono<Profesional> response = personalRepositoryAdapter
                .crearProfesional(this.profesional)
                .thenReturn(this.profesional);

        StepVerifier.create(response)
                .expectNext(this.profesional)
                .verifyComplete();
    }
    @Test
    void ActualizarProfesionalValidoExistenteError(){

        Mockito.when(profesionalRepositoryMock.existsById(this.profesionalData.getNumeroIdentificacion()))
                .thenReturn(Mono.just(Boolean.FALSE));

        Mono<Profesional> response = personalRepositoryAdapter
                .actualizarProfesional(this.profesional)
                .thenReturn(this.profesional);

        StepVerifier.create(response)
                .expectErrorMessage(Mensajes.PROFESIONAL_NO_EXISTE)
                .verify();

    } @Test
    void actualizarProfesionalValido(){

        Mockito.when(profesionalRepositoryMock.existsById(this.profesionalData.getNumeroIdentificacion()))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mockito.when(profesionalRepositoryMock.save(this.profesionalData))
                .thenReturn(Mono.just(this.profesionalData));

        Mockito.when(profesionalRepositoryMock.findById(this.profesionalData.getNumeroIdentificacion()))
                .thenReturn(Mono.just(this.profesionalData));

        Mono<Profesional> response = personalRepositoryAdapter
                .actualizarProfesional(this.profesional)
                .thenReturn(this.profesional);

        StepVerifier.create(response)
                .expectNext(this.profesional)
                .verifyComplete();
    }

    //conductores
    @Test
    void consultarConductores(){
        List<ConductorData> conductoresData = new ArrayList<>(){{add(conductorData);}};

        Mockito.when(conductorRepositoryMock.findAll())
                .thenReturn(Flux.fromIterable(conductoresData));

        Flux<Conductor> response = personalRepositoryAdapter.consultarConductores();

        StepVerifier.create(response)
                .expectNext(conductor)
                .verifyComplete();
    }

    @Test
    void crearConductorExistenteError(){

        Mockito.when(conductorRepositoryMock.existsById(this.conductorData.getNumeroIdentificacion()))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Conductor> response = personalRepositoryAdapter
                .crearConductor(this.conductor)
                .thenReturn(this.conductor);

        StepVerifier.create(response)
                .expectErrorMessage(Mensajes.CONDUCTOR_YA_EXISTE)
                .verify();
    }
    @Test
    void crearConductorValido(){

        Mockito.when(conductorRepositoryMock.existsById(this.conductorData.getNumeroIdentificacion()))
                .thenReturn(Mono.just(Boolean.FALSE));

        Mockito.when(conductorRepositoryMock.insertConductor(this.profesionalData.getNumeroIdentificacion()))
                .thenReturn(Mono.empty());

        Mockito.when(conductorRepositoryMock.save(this.conductorData))
                .thenReturn(Mono.just(this.conductorData));

        Mono<Conductor> response = personalRepositoryAdapter
                .crearConductor(this.conductor)
                .thenReturn(this.conductor);

        StepVerifier.create(response)
                .expectNext(this.conductor)
                .verifyComplete();
    }
    @Test
    void actualizarConductor(){

        Mockito.when(conductorRepositoryMock.existsById(this.conductorData.getNumeroIdentificacion()))
                .thenReturn(Mono.just(Boolean.FALSE));

        Mono<Conductor> response = personalRepositoryAdapter
                .actualizarConductor(this.conductor)
                .thenReturn(this.conductor);

        StepVerifier.create(response)
                .expectErrorMessage(Mensajes.CONDUCTOR_NO_EXISTE)
                .verify();
    }
    @Test
    void actualizarConductorValido(){
        Mockito.when(conductorRepositoryMock.existsById(this.conductorData.getNumeroIdentificacion()))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mockito.when(conductorRepositoryMock.save(this.conductorData))
                .thenReturn(Mono.just(this.conductorData));

        Mono<Conductor> response = personalRepositoryAdapter
                .actualizarConductor(this.conductor)
                .thenReturn(this.conductor);

        StepVerifier.create(response)
                .expectNext(this.conductor)
                .verifyComplete();
    }
    //moviles
    @Test
    void consultarMoviles(){
        List<MovilData> movilesData = new ArrayList<>(){{add(movilData);}};

        Mockito.when(movilRepositoryMock.findAll())
                .thenReturn(Flux.fromIterable(movilesData));

        Flux<Movil> response = personalRepositoryAdapter.consultarMoviles();

        StepVerifier.create(response)
                .expectNext(movil)
                .verifyComplete();
    }

    @Test
    void consultarMovilesByIdRegional(){
        List<MovilData> movilesData = new ArrayList<>(){{add(movilData);}};

        Mockito.when(movilRepositoryMock.findByIdRegional(idRegional))
                .thenReturn(Flux.fromIterable(movilesData));

        Flux<Movil> response = personalRepositoryAdapter.consultarMovilesByIdRegional(idRegional);

        StepVerifier.create(response)
                .expectNext(movil)
                .verifyComplete();
    }

    @Test
    void crearMovilExistenteError(){

        Mockito.when(movilRepositoryMock.existsById(this.movil.getMatricula()))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mono<Movil> response = personalRepositoryAdapter
                .crearMovil(this.movil)
                .thenReturn(this.movil);

        StepVerifier.create(response)
                .expectErrorMessage(Mensajes.MOVIL_YA_EXISTE)
                .verify();
    }
    @Test
    void crearMovilValido(){

        Mockito.when(movilRepositoryMock.existsById(this.movilData.getMatricula()))
                .thenReturn(Mono.just(Boolean.FALSE));


        Mockito.when(movilRepositoryMock.insertMovil(this.movilData.getMatricula()))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mockito.when(movilRepositoryMock.save(this.movilData))
                .thenReturn(Mono.just(this.movilData));

        Mono<Movil> response = personalRepositoryAdapter
                .crearMovil(this.movil)
                .thenReturn(this.movil);

        StepVerifier.create(response)
                .expectNext(this.movil)
                .verifyComplete();
    }

    @Test
    void actualizarMovilNoExistenteError(){

        Mockito.when(movilRepositoryMock.existsById(this.movilData.getMatricula()))
                .thenReturn(Mono.just(Boolean.FALSE));

        Mono<Movil> response = personalRepositoryAdapter
                .actualizarMovil(this.movil)
                .thenReturn(this.movil);

        StepVerifier.create(response)
                .expectErrorMessage(Mensajes.MOVIL_NO_EXISTE)
                .verify();
    }

    @Test
    void actualizarMovilValido(){

        Mockito.when(movilRepositoryMock.existsById(this.movil.getMatricula()))
                .thenReturn(Mono.just(Boolean.TRUE));

        Mockito.when(movilRepositoryMock.save(this.movilData))
                .thenReturn(Mono.just(this.movilData));


        Mono<Movil> response = personalRepositoryAdapter
                .actualizarMovil(this.movil)
                .thenReturn(this.movil);

        StepVerifier.create(response)
                .expectNext(this.movil)
                .verifyComplete();
    }
}
