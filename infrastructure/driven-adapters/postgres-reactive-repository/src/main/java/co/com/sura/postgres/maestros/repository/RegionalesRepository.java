package co.com.sura.postgres.maestros.repository;

import co.com.sura.postgres.maestros.data.RegionalData;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;


public interface RegionalesRepository extends ReactiveCrudRepository<RegionalData,String> {



}
