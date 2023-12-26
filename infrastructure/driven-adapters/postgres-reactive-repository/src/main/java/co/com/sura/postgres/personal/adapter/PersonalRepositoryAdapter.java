package co.com.sura.postgres.personal.adapter;

import co.com.sura.constantes.Mensajes;
import co.com.sura.personal.entity.Conductor;
import co.com.sura.moviles.entity.Movil;
import co.com.sura.personal.entity.Profesional;
import co.com.sura.personal.gateway.PersonalCrudRepository;
import co.com.sura.exception.ErrorValidacionPersonal;
import co.com.sura.postgres.personal.repository.ConductorRepository;
import co.com.sura.postgres.personal.repository.MovilRepository;
import co.com.sura.postgres.personal.repository.ProfesionalRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

import java.time.LocalDate;
import java.util.Comparator;

@Repository
public class PersonalRepositoryAdapter implements PersonalCrudRepository {

    private final ProfesionalRepository profesionalRepository;
    private final MovilRepository movilRepository;
    private final ConductorRepository conductorRepository;

    @Autowired
    public PersonalRepositoryAdapter( ProfesionalRepository profesionalRepository,
                                     MovilRepository movilRepository, ConductorRepository conductorRepository) {

        this.profesionalRepository = profesionalRepository;
        this.movilRepository = movilRepository;
        this.conductorRepository = conductorRepository;

    }

    //profesionales
    @Override
    public Flux<Profesional> consultarProfesionales() {
        return profesionalRepository.findAll()
                .map(ConverterPersonal:: convertToProfesional)
                .sort(Comparator.comparing(Profesional::getNombres));
    }

    @Override
    public Flux<Profesional> consultarProfesionalesByIdRegional(String idRegional) {
        return profesionalRepository.findByIdRegional(idRegional)
                .map(ConverterPersonal:: convertToProfesional)
                .sort(Comparator.comparing(Profesional::getNombres));

    }


    @Override
    public Flux<Profesional> consultarProfesionalByTurnoRegional(LocalDate fechaTurno, String idCiudad) {
        return profesionalRepository.findByTurnoRegional(fechaTurno,idCiudad)
                .map(ConverterPersonal:: convertToProfesional);
    }

    @Override
    public Flux<Profesional> consultarProfesionalFromTurnoRegional(
            LocalDate fechaTurno, String idRegional, Integer idHorarioTurno) {
        return profesionalRepository.findFromTurnoRegional(fechaTurno,idRegional,idHorarioTurno)
                .map(ConverterPersonal:: convertToProfesional);
    }
    @Override
    public Mono<Profesional> crearProfesional(Profesional profesional) {
        return profesionalRepository.existsById(profesional.getNumeroIdentificacion())
                .flatMap(exist ->  {
                    if (Boolean.TRUE.equals(exist)) {
                        return Mono.error(new Throwable(Mensajes.PROFESIONAL_YA_EXISTE));
                    }
                    return profesionalRepository.insertProfesional(profesional.getNumeroIdentificacion())
                            .then(profesionalRepository.save(ConverterPersonal.convertToProfesionalData(profesional)))
                            .then(profesionalRepository.findById(profesional.getNumeroIdentificacion()))
                            .map(ConverterPersonal::convertToProfesional);
                });
    }

    @Override
    public Mono<Profesional> actualizarProfesional(Profesional profesional) {
        return Mono.just(profesional)
                .then(profesionalRepository.existsById(profesional.getNumeroIdentificacion()))
                .flatMap(exist ->{
                    if (Boolean.FALSE.equals(exist)) {
                        return Mono.error(new ErrorValidacionPersonal(Mensajes.PROFESIONAL_NO_EXISTE));
                    }
                    return profesionalRepository.save(ConverterPersonal.convertToProfesionalData(profesional))
                            .then(profesionalRepository.findById(profesional.getNumeroIdentificacion()))
                            .map(ConverterPersonal::convertToProfesional);
                });

    }

    //moviles
    @Override
    public Flux<Movil> consultarMoviles() {
        return movilRepository.findAll()
                .map(ConverterPersonal::convertToMovil);
    }
    @Override
    public Flux<Movil> consultarMovilesSinConductor() {
        return movilRepository.findAllWithoutConductor()
                .map(ConverterPersonal::convertToMovil);
    }
    @Override
    public Flux<Movil> consultarMovilesByIdRegional(String idRegional) {
        return movilRepository.findByIdRegional(idRegional)
                .map(ConverterPersonal::convertToMovil);
    }
    @Override
    public Mono<Movil> crearMovil(Movil movil) {
        return movilRepository.existsById(movil.getMatricula())
                .flatMap(exist ->  {
                    if (Boolean.TRUE.equals(exist)) {
                        return Mono.error(new Throwable(Mensajes.MOVIL_YA_EXISTE));
                    }
                    return movilRepository.save(ConverterPersonal.convertToMovilData(movil))
                            .then(movilRepository.findById(movil.getMatricula()))
                            .map(ConverterPersonal::convertToMovil);
                });
    }

    @Override
    public Mono<Movil> actualizarMovil(Movil movil) {
        return Mono.just(movil)
                .then(movilRepository.existsById(movil.getMatricula()))
                .flatMap(exist ->{
                    if (Boolean.FALSE.equals(exist)) {
                        return Mono.error(new Throwable(Mensajes.MOVIL_NO_EXISTE));
                    }
                    return movilRepository.save(ConverterPersonal.convertToMovilData(movil))
                            .then(movilRepository.findById(movil.getMatricula()))
                            .map(ConverterPersonal::convertToMovil);
                });
    }


    //conductores
    @Override
    public Flux<Conductor> consultarConductores() {
        return conductorRepository.findAll()
                .map(ConverterPersonal::converToConductor);
    }
    @Override
    public Mono<Conductor> crearConductor(Conductor conductor) {
        return  conductorRepository.existsById(conductor.getNumeroIdentificacion())
                .flatMap(exist ->  {
                    if (Boolean.TRUE.equals(exist)) {
                        return Mono.error(new Throwable(Mensajes.CONDUCTOR_YA_EXISTE));
                    }
                    return conductorRepository.insertConductor(conductor.getNumeroIdentificacion())
                            .then(conductorRepository.save(ConverterPersonal.converToConductorData(conductor)))
                            .then(conductorRepository.findById(conductor.getNumeroIdentificacion()))
                            .map(ConverterPersonal::converToConductor);
                });
    }

    @Override
    public Mono<Conductor> actualizarConductor(Conductor conductor) {
        return Mono.just(conductor)
                .then(conductorRepository.existsById(conductor.getNumeroIdentificacion()))
                .flatMap(exist ->{
                    if (Boolean.FALSE.equals(exist)) {
                        return Mono.error(new Throwable(Mensajes.CONDUCTOR_NO_EXISTE));
                    }
                    return conductorRepository.save(ConverterPersonal.converToConductorData(conductor))
                            .then(conductorRepository.findById(conductor.getNumeroIdentificacion()))
                            .map(ConverterPersonal::converToConductor);
                });
    }

}
