package co.com.sura.postgres.maestros.repository;

import co.com.sura.postgres.maestros.data.RegionalesData;
import org.springframework.data.repository.reactive.ReactiveCrudRepository;


public interface RegionalesRepository extends ReactiveCrudRepository<RegionalesData,String> {



}
